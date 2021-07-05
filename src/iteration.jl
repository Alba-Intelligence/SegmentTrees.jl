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


