
"""
Types deriving from `AbstractSegment{T}` must have a `first` and `last`
function each returning a value of type `T`, and `first(i) <= last(i)`
must always be true.
"""
abstract type AbstractSegment{T} end

Base.first(i::AbstractSegment{T}) where T = i.first
Base.last(i::AbstractSegment{T}) where T = i.last

function basic_isless(u::AbstractSegment, v::AbstractSegment)
    return first(u) < first(v) || (first(u) == first(v) && last(u) < last(v))
end

function Base.isless(u::AbstractSegment, v::AbstractSegment)
    return basic_isless(u, v)
end

"""
A basic Segment.
"""
struct Segment{T} <: AbstractSegment{T}
    first::T
    last::T
end
Base.convert(::Type{Segment{T}}, range::AbstractRange{T}) where T =
    Segment(first(range), last(range))
Segment(range::AbstractRange{T}) where T = convert(Segment{T}, range)

"""
An Segment with some associated data.
"""
struct SegmentValue{K, V} <: AbstractSegment{K}
    first::K
    last::K
    value::V
end

SegmentValue(range::AbstractRange{K}, value::V) where {K, V} =
    SegmentValue(first(range), last(range), value)

valtype(::Type{SegmentValue{K,V}}) where {K, V} = V

value(i::SegmentValue{K, V}) where {K, V} = i.value

Base.print(io::IO, x::Segment) = print(io, "\n($(first(x)),$(last(x)))")
function Base.show(io::IO, x::Segment)
    show(io, typeof(x))
    print(io, x)
end

Base.print(io::IO, x::SegmentValue) = print(io, "\n($(first(x)),$(last(x))) => $(value(x))")
function Base.show(io::IO, x::SegmentValue)
    show(io, typeof(x))
    print(io, x)
end

# Each of these types is indexes by K, V, B, where
#   K : Segment type. Segments are represented as (K, K) tuples.
#   V : Segment value type. This an Segment type that may have associated
#        data. It must have `first`, `last`, and `isless` methods.
#   B : Integer giving the B-tree order.

abstract type Node{K, V, B} end


mutable struct InternalNode{K, V, B} <: Node{K, V, B}
    # Internal nodes are keyed by the minimum Segment in the right subtree.  We
    # need internal node keys to be Segments themselves, since ordering by
    # start value alone only works if start values are unique. We don't
    # force that restriction, so we must break ties in order to split nodes that
    # are full of Segments with the same start value.
    keys::Slice{Segment{K}, B}

    # Maximum end ofd this node
    maxend::K

    # Maximum child subtree end-points
    maxends::Slice{K, B}

    children::Slice{Node{K, V, B}, B}

    parent::Union{Nothing, InternalNode{K, V, B}}

    # Sibling/cousin pointers.
    left::Union{Nothing, InternalNode{K, V, B}}
    right::Union{Nothing, InternalNode{K, V, B}}

    function InternalNode{K,V,B}() where {K,V,B}
        return new{K,V,B}(
            Slice{Segment{K}, B}(), zero(K), Slice{K, B}(), Slice{Node{K, V, B}, B}(),
            nothing,
            nothing,
            nothing)
    end
end


function minstart(t::InternalNode)
    return t.keys[1].first
end


mutable struct LeafNode{K, V, B} <: Node{K, V, B}
    entries::Vector{V}
    keys::Vector{Segment{K}}
    count::UInt32 # number of stored entries

    # maximum Segment end-point in this leaf node.
    maxend::K

    parent::Union{Nothing, InternalNode{K, V, B}}

    # Sibling/cousin pointers.
    left::Union{Nothing, LeafNode{K, V, B}}
    right::Union{Nothing, LeafNode{K, V, B}}

    function LeafNode{K,V,B}() where {K,V,B}
        return new{K,V,B}(
            Array{V}(undef, B), Array{Segment{K}}(undef, B), 0,
            zero(K),
            nothing,
            nothing,
            nothing)
    end
end


function minstart(t::LeafNode)
    return first(t.keys[1])
end


function Base.resize!(leaf::LeafNode{K, V, B}, n) where {K, V, B}
    @assert 0 <= n <= B
    leaf.count = n
end


function Base.splice!(leaf::LeafNode{K, V, B}, i::Integer) where {K, V, B}
    slice_splice!(leaf.keys, leaf.count, i)
    x, leaf.count = slice_splice!(leaf.entries, leaf.count, i)
    return x
end


function Base.insert!(leaf::LeafNode{K, V, B}, i::Integer, value::V) where {K, V, B}
    slice_insert!(leaf.keys, leaf.count, i, Segment{K}(first(value), last(value)))
    slice_insert!(leaf.entries, leaf.count, i, value)
    leaf.count += 1
end


function Base.push!(leaf::LeafNode{K, V, B}, value::V) where {K, V, B}
    leaf.count += 1
    leaf.entries[leaf.count] = value
    leaf.keys[leaf.count] = Segment{K}(first(value), last(value))
end


function Base.pop!(leaf::LeafNode{K, V, B}) where {K, V, B}
    x = leaf.entries[leaf.count]
    leaf.count -= 1
    return x
end

function Base.iterate(leaf::LeafNode, i::Int=1)
    return i ≤ length(leaf) ? (leaf.entries[i], i + 1) : nothing
end


mutable struct SegmentBTree{K, V, B}
    root::Node{K, V, B}
    n::Int # Number of entries

    function SegmentBTree{K,V,B}() where {K,V,B}
        return new{K,V,B}(LeafNode{K, V, B}(), 0)
    end

    # Construct an Segment tree from a sorted array of Segments.
    #
    # This is generally much more efficient than constructing the same tree by
    # inserting Segments one by one.
    #
    # Args:
    #   entries: Segment entry values in sorted order.
    #
    function SegmentBTree{K,V,B}(entries::AbstractVector{V}) where {K,V,B}
        if !issorted(entries, lt=basic_isless)
            error("Segments must be sorted to construct an SegmentTree")
        end

        # Here's the plan: figure out how many leaf nodes we need, allocate
        # them all up front. Copy in the keys and values, then work up towards
        # the root.

        n = length(entries)

        if n == 0
            return new{K,V,B}(LeafNode{K, V, B}(), 0)
        end

        numleaves = cld(n, B - 2)
        leaves = [LeafNode{K, V, B}() for _ in 1:numleaves]

        maxends = Vector{K}(undef, numleaves)
        minkeys = Vector{Segment{K}}(undef, numleaves)

        # divy up the keys and values among the leaves
        keys_per_leaf = cld(n, numleaves)
        for i in 1:numleaves
            u = (i - 1) * keys_per_leaf + 1
            v = min(n, i * keys_per_leaf)
            minkeys[i] = Segment{K}(first(entries[u]), last(entries[u]))
            maxends[i] = last(entries[u])
            for j in u:v
                push!(leaves[i], entries[j])
                maxends[i] = max(maxends[i], last(entries[j]))
            end
            leaves[i].maxend = maxends[i]
        end

        children = leaves
        while length(children) > 1
            # sibling pointers
            for i in 1:length(children)-1
                children[i].right = children[i+1]
                children[i+1].left = children[i]
            end

            # make parents
            numparents = cld(length(children), B - 2)
            parents = [InternalNode{K, V, B}() for _ in 1:numparents]

            # divy up children among parents
            children_per_parent = cld(length(children), numparents)
            for i in 1:numparents
                u = (i - 1) * keys_per_leaf + 1
                v = min(length(children), i * keys_per_leaf)
                maxend = maxends[u]
                for j in u:v
                    push!(parents[i].children, children[j])
                    push!(parents[i].maxends, maxends[j])
                    children[j].parent = parents[i]
                    maxend = max(maxend, maxends[j])
                    if j > u
                        push!(parents[i].keys, minkeys[j])
                    end
                end
                minkeys[i] = minkeys[u]
                parents[i].maxend = maxend
                maxends[i] = maxend
            end

            children = parents
        end
        @assert length(children) == 1

        return new{K,V,B}(children[1], n)
    end
end


# Default B-tree order
const SegmentTree{K, V} = SegmentBTree{K, V, 64}

# Show

function Base.show(io::IO, it::SegmentTree)
    show(io, typeof(it))
    n = length(it)
    if length(it) > 6
        # Hacky random access ...
        for (i, x) in enumerate(it)
            i < 4 && print(x)
        end
        print("\n\u22EE") # Vertical ellipsis
        for (i, x) in enumerate(it)
            i > (n-3) && print(x)
        end
    else
        for x in it
            print(x)
        end
    end
end



# Length
# ------

function Base.length(t::SegmentBTree)
    return t.n
end


function depth(t::SegmentBTree)
    k = 1
    node = t.root
    while !isa(node, LeafNode)
        k += 1
        node = node.children[1]
    end
    return k
end


function Base.length(t::InternalNode)
    return length(t.children)
end


function Base.length(t::LeafNode)
    return t.count
end


function Base.isempty(t::SegmentBTree)
    return t.n == 0
end


function Base.isempty(t::InternalNode)
    return isempty(t.children)
end


function Base.isempty(t::LeafNode)
    return t.count == 0
end


