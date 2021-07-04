
# SegmentTree map operations

const SegmentMap{K, V} = SegmentTree{K, SegmentValue{K, V}}


function Base.setindex!(t::SegmentBTree{K, V, B}, value,
                        key::Tuple{Any, Any}) where {K, V, B}
    push!(t, V(key[1], key[2], value), true)
end


function Base.setindex!(t::SegmentBTree{K, V, B}, value,
                        key::Segment{K}) where {K, V, B}
    push!(t, V(key.first, key.last, value), true)
end


function Base.setindex!(t::SegmentBTree{K, V, B}, value,
                        first, last) where {K, V, B}
    push!(t, V(first, last, value), true)
end


function Base.getindex(t::SegmentBTree{K, V, B}, key::AbstractSegment{K}) where {K, V, B}
    return _getindex(t.root, key)
end


function _getindex(t::InternalNode{K, V, B}, key::AbstractSegment{K}) where {K, V, B}
    i = findidx(t, key)
    if 1 <= length(t) - 1 && key >= t.keys[i]
        return _getindex(t.children[i+1], key)
    else
        return _getindex(t.children[i], key)
    end
end


function _getindex(t::LeafNode{K, V, B}, key::AbstractSegment{K}) where {K, V, B}
    i = findidx(t, key)
    if 1 <= i <= length(t) && first(t.entries[i]) == first(key) &&
        last(t.entries[i]) == last(key)
        return t.entries[i]
    else
        error(KeyError((first(key), last(key))))
    end
end


function Base.get(t::SegmentBTree{K, V, B}, key::Tuple{Any, Any}, default) where {K, V, B}
    return _get(t.root, Segment{K}(key[1], key[2]), default)
end


function Base.get(t::SegmentBTree{K, V, B}, key::AbstractSegment{K}, default) where {K, V, B}
    return _get(t.root, key, default)
end


function _get(t::InternalNode{K, V, B}, key::AbstractSegment{K}, default) where {K, V, B}
    i = findidx(t, key)
    if 1 <= length(t) - 1 && key >= t.keys[i]
        return _get(t.children[i+1], key, default)
    else
        return _get(t.children[i], key, default)
    end
end


function _get(t::LeafNode{K, V, B}, key::AbstractSegment{K}, default) where {K, V, B}
    i = findidx(t, key)
    if 1 <= i <= length(t) &&
        first(t.entries[i]) == first(key) && last(t.entries[i]) == last(key)
        return t.entries[i]
    else
        return default
    end
end


function Base.get!(t::SegmentBTree{K, V, B}, key::Tuple{Any, Any}, default) where {K, V, B}
    return push!(t, V(key[1], key[2], default), true, false)
end


function Base.get!(t::SegmentBTree{K, V, B}, key::AbstractSegment{K}, default) where {K, V, B}
    return push!(t, V(first(key), last(key), default), true, false)
end


function Base.delete!(t::SegmentBTree{K, V, B}, first, last) where {K, V, B}
    return deletefirst!(t, first, last)
end


function Base.delete!(t::SegmentBTree{K, V, B}, key::Tuple{Any, Any}) where {K, V, B}
    return deletefirst!(t, key)
end


function Base.delete!(t::SegmentBTree{K, V, B}, key::Segment{K}) where {K, V, B}
    return deletefirst!(t, key)
end


struct SegmentKeyIterator{K, V, B}
    t::SegmentBTree{K, V, B}
end

Base.eltype(::Type{SegmentKeyIterator{K,V,B}}) where {K,V,B} = Segment{K}
Base.IteratorSize(::Type{SegmentKeyIterator{K,V,B}}) where {K,V,B} = Base.SizeUnknown()


function Base.keys(t::SegmentBTree)
    return SegmentKeyIterator(t)
end

function Base.iterate(
        it::SegmentKeyIterator{K, V, B},
        state::SegmentBTreeIteratorState{K, V, B}=iterinitstate(it)) where {K, V, B}
    if state.leaf === nothing || isempty(notnothing(state.leaf))
        return nothing
    end
    leaf = state.leaf
    key = Segment{K}(first(leaf.entries[state.i]), last(leaf.entries[state.i]))
    if state.i < length(leaf)
        state = SegmentBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = SegmentBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return key, state
end

function iterinitstate(it::SegmentKeyIterator{K, V, B}) where {K, V, B}
    # traverse to the first leaf node
    node = it.t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end
    return SegmentBTreeIteratorState(node, 1)
end


struct SegmentValueIterator{K, V <: SegmentValue, B}
    t::SegmentBTree{K, V, B}
end

Base.eltype(::Type{SegmentValueIterator{K,V,B}}) where {K,V,B} = valtype(V)
Base.IteratorSize(::Type{SegmentValueIterator{K,V,B}}) where {K,V,B} = Base.SizeUnknown()


function Base.values(t::SegmentBTree{K, V, B}) where {K, V<:SegmentValue, B}
    return SegmentValueIterator(t)
end

function Base.iterate(
        it::SegmentValueIterator{K, V, B},
        state::SegmentBTreeIteratorState{K, V, B}=iterinitstate(it)) where {K, V, B}
    if state.leaf === nothing || isempty(notnothing(state.leaf))
        return nothing
    end
    leaf = state.leaf
    value = leaf.entries[state.i].value
    if state.i < length(leaf)
        state = SegmentBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = SegmentBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return value, state
end

function iterinitstate(it::SegmentValueIterator{K, V, B}) where {K, V, B}
    # traverse to the first leaf node
    node = it.t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end
    return SegmentBTreeIteratorState(node, 1)
end
