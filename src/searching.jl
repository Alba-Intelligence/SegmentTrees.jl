
# Searching
# ---------

function findidx(t::LeafNode{K, V, B}, key::AbstractSegment{K}) where {K, V, B}
    if isempty(t)
        return 0
    end
    # TODO: search keys
    return searchsortedfirst(t.entries, key, 1, Int(t.count),
                             Base.ord(basic_isless, identity, false))
end

function findidx(t::InternalNode{K, V, B}, key::AbstractSegment{K}) where {K, V, B}
    if isempty(t.keys)
        return 0
    end
    i = searchsortedfirst(t.keys, key)
    return min(i, length(t.keys))
end

function Base.haskey(t::LeafNode{K, V, B}, key::AbstractSegment{K}) where {K, V, B}
    i = findidx(t, key)
    return 1 <= i <= length(t) && first(t.keys[i]) == first(key) &&
           last(t.keys[i]) == last(key)
end


function Base.haskey(t::InternalNode{K, V, B}, key::AbstractSegment{K}) where {K, V, B}
    i = findidx(t, key)
    if i <= length(t) - 1 && !(key < t.keys[i])
        return haskey(t.children[i+1], key)
    else
        return haskey(t.children[i], key)
    end
end


function Base.haskey(t::SegmentBTree{K, V, B}, key0::Tuple{Any, Any}) where {K, V, B}
    key = Segment{K}(key0[1], key0[2])
    return haskey(t.root, key)
end


function Base.findfirst(t::LeafNode{K, V, B}, key::AbstractSegment{K}, f) where {K, V, B}
    i = findidx(t, key)
    while 1 <= i <= length(t) &&
          first(t.keys[i]) == first(key) &&
          last(t.keys[i]) == last(key)
        if f(t.entries[i], key)
            return t.entries[i]
        end
        i += 1
        if i > length(t) && t.right !== nothing
            t = notnothing(t.right)
            i = 1
        end
    end

    return nothing
end


function Base.findfirst(t::InternalNode{K, V, B}, key::AbstractSegment{K}, f) where {K, V, B}
    i = findidx(t, key)
    if i <= length(t) - 1 && !(key < t.keys[i])
        return findfirst(t.children[i+1], key, f)
    else
        return findfirst(t.children[i], key, f)
    end
end


true_cmp(a, b) = true

function Base.findfirst(t::SegmentBTree{K, V, B}, key::AbstractSegment{K},
                        f=true_cmp) where {K, V, B}
    return findfirst(t.root, key, f)
end


function Base.findfirst(t::SegmentBTree{K, V, B}, key0::Tuple{Any, Any},
                        f=true_cmp) where {K, V, B}
    return findfirst(t, Segment{K}(key0[1], key0[2]), f)
end

