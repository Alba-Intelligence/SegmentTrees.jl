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


# Iterating
# ---------

mutable struct SegmentBTreeIteratorState{K, V, B}
    leaf::Union{Nothing, LeafNode{K, V, B}}
    i::Int
end

function Base.iterate(
        t::SegmentBTree{K, V, B},
        state::SegmentBTreeIteratorState{K, V, B}=iterinitstate(t)) where {K, V, B}
    if state.leaf === nothing || isempty(state.leaf)
        return nothing
    end
    leaf = notnothing(state.leaf)
    entry = leaf.entries[state.i]
    if state.i < length(leaf)
        state.i += 1
    else
        state.leaf = leaf.right
        state.i = 1
    end
    return entry, state
end

function iterinitstate(t::SegmentBTree{K,V,B}) where {K, V, B}
    # traverse to the first leaf node
    node = t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end
    return SegmentBTreeIteratorState(node, 1)
end


# Iterate from a given starting from the first Segment that intersects a given point.
struct SegmentFromIterator{K, V, B}
    t::SegmentBTree{K, V, B}
    p::K
end

Base.eltype(::Type{SegmentFromIterator{K, V, B}}) where {K, V, B} = V
Base.IteratorSize(::Type{SegmentFromIterator{K, V, B}}) where {K, V, B} = Base.SizeUnknown()

function from(t::SegmentBTree{K, V, B}, p) where {K, V, B}
    return SegmentFromIterator{K, V, B}(t, convert(K, p))
end

function Base.iterate(
        it::SegmentFromIterator{K, V, B},
        state::SegmentBTreeIteratorState{K, V, B}=iterinitstate(it)) where {K, V, B}
    return iterate(it.t, state)
end

function iterinitstate(it::SegmentFromIterator{K, V, B}) where {K, V, B}
    node, i = firstfrom(it.t, it.p)
    if i == 0
        return SegmentBTreeIteratorState{K, V, B}(nothing, i)
    else
        return SegmentBTreeIteratorState{K, V, B}(node, i)
    end
end


# Inserting
# ---------


# Split a leaf into two, returning (leftnode, rightnode)
function split!(left::LeafNode{K, V, B}) where {K, V, B}
    right = LeafNode{K, V, B}()
    right.right = left.right
    right.left = left
    left.right = right
    if right.right !== nothing
        right.right = notnothing(right.right)
        right.right.left = right
    end
    right.parent = left.parent

    m = length(left)
    resize!(right, m - div(m, 2))
    copyto!(right.entries, 1, left.entries, div(m, 2) + 1, length(right))
    copyto!(right.keys, 1, left.keys, div(m, 2) + 1, length(right))
    resize!(left, div(m, 2))

    left.maxend = last(left.keys[1])
    for entry in left
        left.maxend = max(left.maxend, last(entry))
    end

    right.maxend = last(right.keys[1])
    for entry in right
        right.maxend = max(right.maxend, last(entry))
    end

    return left, right
end


# Split an internal node in two, returning (leftnode, rightnode)
function split!(left::InternalNode{K, V, B}) where {K, V, B}
    right = InternalNode{K, V, B}()
    right.right = left.right
    right.left = left
    left.right = right
    if right.right !== nothing
        right.right = notnothing(right.right)
        right.right.left = right
    end
    right.parent = left.parent

    m = length(left)

    resize!(right.children, m - div(m, 2))
    resize!(right.maxends, m - div(m, 2))
    resize!(right.keys, m - div(m, 2) - 1)

    copyto!(right.children, 1, left.children, div(m, 2)+1, length(right.children))
    copyto!(right.maxends, 1, left.maxends, div(m, 2)+1, length(right.maxends))
    copyto!(right.keys, 1, left.keys, div(m, 2)+1, length(right.keys))

    resize!(left.children, div(m, 2))
    resize!(left.maxends, div(m, 2))
    resize!(left.keys, div(m, 2) - 1)

    left.maxend = left.children[1].maxend
    for child in left.children
        left.maxend = max(left.maxend, child.maxend)
    end

    right.maxend = right.children[1].maxend
    for child in right.children
        right.maxend = max(right.maxend, child.maxend)
    end

    for child in right.children
        child.parent = right
    end

    return left, right
end


# Find the maximum Segment end point is a subtree
function nodemaxend(t::InternalNode{K, V, B}) where {K, V, B}
    maxend = zero(K)
    for i in 1:length(t.children)
        maxend = max(maxend, t.children[i].maxend)
    end
    return maxend
end


function nodemaxend(t::LeafNode{K, V, B}) where {K, V, B}
    maxend = zero(K)
    for i in 1:length(t)
        maxend = max(maxend, last(t.keys[i]))
    end
    return maxend
end


sameparent(u::Node{K, V, B}, v::Node{K, V, B}) where {K, V, B} = (u.parent == v.parent)

# Find the first leaf node in the tree
function firstleaf(t::SegmentBTree)
    return firstleaf(t.root)
end


function firstleaf(t::InternalNode)
    return firstleaf(t.children[1])
end


function firstleaf(t::LeafNode)
    return t
end


# Find the root node
function root(t::Node)
    while t.parent !== nothing
        t = notnothing(t.parent)
    end
    return t
end


# This version of nextleafkey is used in nextintersection at aggressively avoid
# allocation, and dereferecing nullables more than once.
macro nextleafkey(leaf, nleaf, i)
    quote
        if $(esc(i)) < length($(esc(leaf)))
            $(esc(i)) += 1
        else
            $(esc(i)) = 1
            $(esc(nleaf)) = $(esc(leaf)).right
        end
    end
end


# Find the minimum Segment in a subtree
function minkey(t::InternalNode)
    return minkey(t.children[1])
end

function minkey(t::LeafNode{K, V, B}) where {K, V, B}
    return t.keys[1]
end


function Base.push!(t::SegmentBTree{K, V, B}, entry::V,
                    unique_key::Bool=false, update::Bool=true) where {K, V, B}
    return _push!(t, t.root, entry, unique_key, update)
end


function _push!(t::SegmentBTree{K, V, B},
                node::InternalNode{K, V, B},
                entry::V, unique_key::Bool, update::Bool) where {K, V, B}
    i = findidx(node, entry) # key index
    j = i <= length(node) - 1 && entry < node.keys[i] ? i : i + 1 # child index
    return _push!(t, node.children[j], entry, unique_key, update)
end


function _push!(t::SegmentBTree{K, V, B},
                node::LeafNode{K, V, B}, entry::V,
                unique_key::Bool, update::Bool) where {K, V, B}
    i = max(1, findidx(node, entry))

    # key exists and we are replacing it
    if i <= length(node) && unique_key &&
        first(node.keys[i]) == first(entry) && last(node.keys[i]) == last(entry)
        if update
            node.entries[i] = entry
            return entry
        else
            return node.entries[i]
        end
    end

    insert!(node, i, entry)
    if length(node) == 1 || last(entry) > node.maxend
        node.maxend = last(entry)
    end
    t.n += 1

    # split when full
    if length(node) == B
        maxend = node.maxend
        leftleaf, rightleaf = split!(node)
        median = rightleaf.keys[1]

        # travel back up the tree setting maxend values and splitting as needed
        parent = node.parent
        leftnode = leftleaf
        child = leftleaf
        rightnode = rightleaf
        hassplit = true
        while parent !== nothing
            p = notnothing(parent)
            if hassplit
                i = findidx(p, entry) # key index
                j = i <= length(p) - 1 && entry < p.keys[i] ? i : i + 1 # child index
                p.maxends[j] = p.children[j].maxend
                insert!(p.children, j + 1, rightnode)
                insert!(p.maxends, j + 1, p.children[j+1].maxend)
                p.maxend = max(p.maxend, maxend)
                insert!(p.keys, j, median)

                # split when full
                if length(p) == B
                    leftnode, rightnode = split!(p)
                    median = minkey(rightnode)
                    maxend = max(leftnode.maxend, rightnode.maxend)
                else
                    hassplit = false
                end
            else
                p.maxend = max(p.maxend, last(entry))
                ifind = findfirst(isequal(child), p.children)
                if ifind === nothing
                    ifind = 0
                end
                p.maxends[ifind] = child.maxend
            end
            child = p
            parent = p.parent
        end

        if hassplit
            t.root = InternalNode{K, V, B}()
            push!(t.root.keys, median)
            push!(t.root.children, leftnode)
            push!(t.root.children, rightnode)
            t.root.maxend = maxend
            push!(t.root.maxends, leftnode.maxend)
            push!(t.root.maxends, rightnode.maxend)
            leftnode.parent = t.root
            rightnode.parent = t.root
        end
    else
        # travel back up the tree setting maxend values
        parent = node.parent
        child = node
        while parent !== nothing
            p = notnothing(parent)
            p.maxend = max(p.maxend, last(entry))
            ifind = findfirst(isequal(child), p.children)
            if ifind === nothing
                ifind = 0
            end
            p.maxends[ifind] = child.maxend
            parent = p.parent
            child = p
        end
    end

    return entry
end


