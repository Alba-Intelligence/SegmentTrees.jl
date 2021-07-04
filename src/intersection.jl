# Intersection
# ------------

# There are two ways we can tackle intersection: traversal (search the tree for
# Segments that intersect) and iteration (iterate through the tree's Segments
# in sorted order).
#
# Traversal is roughly O(log(n)) while iteration is roughly O(n). Obviously
# if we are doing a single intersection, we would choose traversal, but when
# intersecting two trees it becomes more interesting.
#
# If the second tree is of size m, then intersection by traversal is
# O(min(m * log(n), n * log(m))) while interesction by iteration is O(m + n).
# When m and n are both large, it becomes more efficient to intersect by
# iteration, but deciding when requires some heuristics.
#

# Return true iff two key1 and key2 intersect.
@inline function intersects(key1::AbstractSegment{K}, key2::AbstractSegment{K}) where K
    return first(key1) <= last(key2) && first(key2) <= last(key1)
end


# Return true if the tree has an intersection with the given position
function hasintersection(t::SegmentBTree{K, V, B}, query) where {K, V, B}
    return hasintersection(t.root, convert(K, query))
end


function hasintersection(t::InternalNode{K, V, B}, query::K) where {K, V, B}
    if isempty(t) || t.maxend < query
        return false
    end

    for (i, child) in enumerate(t.children)
        if child.maxend >= query && (i == 1 || t.keys[i-1].first <= query)
            if hasintersection(child, query)
                return true
            end
        elseif minstart(child) > query
            break
        end
    end

    return false
end


function hasintersection(t::LeafNode{K, V, B}, query::K) where {K, V, B}
    if isempty(t) || t.maxend < query
        return false
    end

    for i in 1:length(t)
        if first(t.keys[i]) <= query <= last(t.keys[i])
            return true
        elseif query < first(t.keys[i])
            break
        end
    end

    return false
end


# Represent an intersection in an SegmentTree by pointing to a LeafNode
# and an index within that leaf node. No intersection is represented with
# index == 0.
mutable struct Intersection{K, V, B}
    index::Int
    node::LeafNode{K, V, B}

    Intersection{K,V,B}(index, node) where {K,V,B} = new{K,V,B}(index, node)
    Intersection{K,V,B}() where {K,V,B} = new{K,V,B}(0)
end


# Find the first Segment in the tree that intersects the query and return
# as a (leafnode, index) pair, indicating that leafnode.keys[index] intersects.
# If no intersection is found, index is 0 and leafnode is the last node
# searched.
function firstintersection!(t::SegmentBTree{K, V, B},
                            query::AbstractSegment{K},
                            lower::Union{Nothing, V},
                            out::Intersection{K, V, B},
                            filter::F) where {F, K, V, B}
    return firstintersection!(t.root, query, lower, out, filter)
end


function firstintersection!(t::InternalNode{K, V, B},
                            query::AbstractSegment{K},
                            lower::Union{Nothing, V},
                            out::Intersection{K, V, B},
                            filter::F) where {F, K, V, B}
    if isempty(t) || t.maxend < first(query)
        out.index = 0
        return
    end

    (query_first, query_last) = (first(query), last(query))

    i = lower === nothing ? 1 : searchsortedfirst(t.keys, notnothing(lower))

    @inbounds while i <= length(t.children)
        if i > 1 && t.keys[i-1].first > query_last
            break
        end
        if t.maxends[i] >= query_first
            firstintersection!(t.children[i], query, lower, out, filter)
            if out.index > 0
                return
            end
        end
        i += 1
    end

    out.index = 0
    return
end


function firstintersection!(t::LeafNode{K, V, B},
                            query::AbstractSegment{K},
                            lower::Union{Nothing, V},
                            out::Intersection{K, V, B},
                            filter::F) where {F, K, V, B}
    if isempty(t) || t.maxend < first(query)
        out.index = 0
        return
    end

    # TODO: search keys
    i = lower === nothing ? 1 :
        searchsortedfirst(t.entries, notnothing(lower), 1, Int(t.count),
                          Base.ord(basic_isless, identity, false))

    while i <= length(t)
        if intersects(t.entries[i], query) && filter(t.entries[i], query)
            out.index = i
            out.node = t
            return
        elseif last(query) < first(t.keys[i])
            break
        end
        i += 1
    end

    out.index = 0
    return
end


function firstintersection(t::InternalNode{K, V, B},
                           query::Segment{K},
                           lower::Union{Nothing, Segment{K}}) where {K, V, B}
    if isempty(t) || t.maxend < first(query)
        return (nothing, 1)
    end

    i = lower === nothing ? 1 : searchsortedfirst(t.keys, notnothing(lower))

    @inbounds while i <= length(t.children)
        if i > 1 && t.keys[i-1].first > last(query)
            break
        end
        if t.maxends[i] >= first(query)
            w, k = firstintersection(t.children[i], query, lower)
            w !== nothing && return notnothing(w), k
        end
        i += 1
    end

    return (nothing, 1)
end


function firstintersection(t::LeafNode{K, V, B}, query::Segment{K},
                           lower::Union{Nothing, Segment{K}}) where {K, V, B}

    if isempty(t) || t.maxend < first(query)
        return (nothing, 1)
    end

    i = lower === nothing ? 1 : searchsortedfirst(t.keys, notnothing(lower))

    while i <= length(t)
        if intersects(t.keys[i], query) &&
           (lower === nothing || !basic_isless(t.keys[i], notnothing(lower)))
            return (t, i)
        elseif last(query) < first(t.keys[i])
            break
        end
        i += 1
    end

    return (nothing, 1)
end


# Find the first key with an end point >= query
function firstfrom(t::SegmentBTree{K, V, B}, query::K) where {K, V, B}
    return firstfrom(t.root, query)
end


function firstfrom(t::InternalNode{K, V, B}, query::K) where {K, V, B}
    if isempty(t)
        return (t, 0)
    end

    for (i, child) in enumerate(t.children)
        if child.maxend >= query
            return firstfrom(child, query)
        end
    end

    return (t, 0)
end


function firstfrom(t::LeafNode{K, V, B}, query::K) where {K, V, B}
    if isempty(t)
        return (t, 0)
    end

    for i in 1:length(t)
        if last(t.keys[i]) >= query
            return (t, i)
        end
    end

    return (t, 0)
end



# If query intersects node.keys[i], return the next intersecting key as
# a (leafnode, index) pair.
function nextintersection!(t::LeafNode{K, V, B}, i::Integer,
                           query::AbstractSegment{K},
                           out::Intersection{K, V, B},
                           filter::F) where {F, K, V, B}
    j = i + 1
    while true
        while j <= length(t)
            if intersects(t.keys[j], query) && filter(t.entries[j], query)
                out.index = j
                out.node = t
                return
            end
            j += 1
        end
        j = 1

        t.right === nothing && break

        t = notnothing(t.right)

        if minstart(t) > last(query)
            break
        end

        # When we hit a leaf node with no possible intersections, we start over
        # from the top of the leaf. This is a heuristic to avoid linear search
        # behavior in the presence of extremely long Segments.
        if t.maxend < first(query)
            return firstintersection!(root(t), query,
                                      t.entries[t.count], out, filter)
        end
    end

    out.index = 0
    return
end


mutable struct SegmentIntersectionIterator{F, K, V, B}
    filter::F
    intersection::Intersection{K, V, B}
    t::SegmentBTree{K, V, B}
    query::AbstractSegment{K}

    function SegmentIntersectionIterator{F,K,V,B}(filter, intersection, t, query) where {F,K,V,B}
        return new{F,K,V,B}(filter, intersection, t, query)
    end

    function SegmentIntersectionIterator{F,K,V,B}() where {F,K,V,B}
        return new{F,K,V,B}(Intersection{F, K, V, B}())
    end
end

Base.eltype(::Type{SegmentIntersectionIterator{F,K,V,B}}) where {F,K,V,B} = V
Base.IteratorSize(::Type{SegmentIntersectionIterator{F,K,V,B}}) where {F,K,V,B} = Base.SizeUnknown()

# Intersect an Segment tree t with a single Segment, returning an iterator
# over the intersecting (key, value) pairs in t.
function Base.intersect(t::SegmentBTree{K, V, B}, query0::Tuple{Any, Any},
                        filter::F=true_cmp) where {F, K, V, B}
    query = Segment{K}(query0[1], query0[2])
    return intersect(t, query, filter)
end


function Base.intersect(t::SegmentBTree{K, V, B}, first::K, last::K,
                        filter::F=true_cmp) where {F, K, V, B}
    query = Segment{K}(first, last)
    return intersect(t, query, filter)
end


function Base.intersect(t::SegmentBTree{K, V, B}, query::AbstractSegment{K},
                        filter::F=true_cmp) where {F, K, V, B}
    return SegmentIntersectionIterator{F, K, V, B}(filter, Intersection{K, V, B}(), t, query)
end



function Base.iterate(it::SegmentIntersectionIterator{F, K, V, B}) where {F, K, V, B}
    firstintersection!(it.t, it.query, nothing, it.intersection, it.filter)
    return iterate(it, nothing)
end

function Base.iterate(it::SegmentIntersectionIterator{F, K, V, B}, _) where {F, K, V, B}
    intersection = it.intersection
    if intersection.index == 0
        return nothing
    end
    entry = intersection.node.entries[intersection.index]
    nextintersection!(intersection.node, intersection.index,
                      it.query, intersection, it.filter)
    return entry, nothing
end


mutable struct IntersectionIterator{F, K, V1, B1, V2, B2}
    filter::F

    t1::SegmentBTree{K, V1, B1}
    t2::SegmentBTree{K, V2, B2}

    # if true, use successive intersection, if false iterative.
    successive::Bool

    # true if done
    isdone::Bool

    # intersection state
    u::Union{Nothing, LeafNode{K, V1, B1}}
    v::Union{Nothing, LeafNode{K, V2, B2}}
    w::Union{Nothing, LeafNode{K, V2, B2}}
    i::Int
    j::Int
    k::Int

    function IntersectionIterator{F,K,V1,B1,V2,B2}(filter::F, t1, t2, successive::Bool) where {F,K,V1,B1,V2,B2}
        return new{F,K,V1,B1,V2,B2}(filter, t1, t2, successive)
    end
end

Base.eltype(::Type{IntersectionIterator{F,K,V1,B1,V2,B2}}) where {F,K,V1,B1,V2,B2} = Tuple{V1,V2}
Base.IteratorSize(::Type{IntersectionIterator{F,K,V1,B1,V2,B2}}) where {F,K,V1,B1,V2,B2} = Base.SizeUnknown()

function Base.iterate(it::IntersectionIterator, _=iterinitstate(it))
    if it.isdone
        return nothing
    end
    if it.successive
        u = it.u
        w = it.w
        value = (u.entries[it.i], w.entries[it.k])
        @nextleafkey(w, it.w, it.k)
        successive_nextintersection!(it)
    else
        u = it.u
        w = it.w
        value = (u.entries[it.i], w.entries[it.k])
        @nextleafkey(w, it.w, it.k)
        iterative_nextintersection!(it)
    end
    return value, nothing
end

function iterinitstate(it::IntersectionIterator)
    it.isdone = true
    if it.successive
        it.u = isempty(it.t1) ? nothing : firstleaf(it.t1)
        it.w = isempty(it.t2) ? nothing : firstleaf(it.t2)
        it.i, it.k = 1, 1
        successive_nextintersection!(it)
    else
        # Iterative Intersection: Intersect by iterating through the two
        # collections in unison.
        #
        # Notation notes. We proceed by finding all the the Segments
        # in t2 that intersect u.keys[i], before proceeding to the next
        # key in t1.
        #
        # We use v to keep track of the first leaf node in t2 that might contain an
        # intersecting Segment, # and with (w, j) the current intersecting entry in
        # t2. We need to hang onto v since we will need to jump back when the
        # entry in t1 gets incremented.
        #
        # The thing to keep in mind is that this is just like the merge operation in
        # mergesort, except that some backtracking is needed since intersection
        # isn't as simple as ordering.

        it.u = isempty(it.t1) ? nothing : firstleaf(it.t1)
        it.v = isempty(it.t2) ? nothing : firstleaf(it.t2)
        it.w = it.v
        it.i, it.j, it.k = 1, 1, 1
        iterative_nextintersection!(it)
    end
    return nothing  # iterator is stateful
end

function iterative_nextintersection!(
    it::IntersectionIterator{F, K, V1, B1, V2, B2}) where {F, K, V1, B1, V2, B2}

    u, v, w, i, j, k = it.u, it.v, it.w, it.i, it.j, it.k

    it.isdone = true

    while true
        u === nothing && return
        unode = notnothing(u)
        ukey = unode.keys[i]

        v === nothing && return
        vnode = notnothing(v)
        vkey = vnode.keys[j]

        # find next intersection w
        while w !== nothing
            wnode = notnothing(w)
            wkey = wnode.keys[k]

            if first(wkey) <= last(ukey)
                if first(ukey) <= last(wkey) && it.filter(unode.entries[i], wnode.entries[k])
                    it.isdone = false
                    break
                end
                @nextleafkey(wnode, w, k)
            else
                break
            end
        end

        # intersection was found
        if !it.isdone
            break
        end

        # if no intersection found, advance u and start again
        @nextleafkey(unode, u, i)

        u === nothing && break

        # find new v corresponding to new u
        unode = notnothing(u)
        ukey = unode.keys[i]
        while v !== nothing
            vnode = notnothing(v)
            vkey = vnode.keys[j]

            if last(vkey) < first(ukey)
                @nextleafkey(vnode, v, j)
            else
                break
            end
        end

        w, k = v, j
    end

    it.u = u
    it.v = v
    it.w = w
    it.i = i
    it.j = j
    it.k = k
    return
end


function successive_nextintersection!(
    it::IntersectionIterator{F, K, V1, B1, V2, B2}) where {F, K, V1, B1, V2, B2}

    u, w, i, k = it.u, it.w, it.i, it.k

    it.isdone = true

    while true
        u === nothing && return
        unode = notnothing(u)
        ukey = unode.keys[i]

        # find next intersection w
        while w !== nothing
            wnode = notnothing(w)
            wkey = wnode.keys[k]

            if first(wkey) <= last(ukey)
                if first(ukey) <= last(wkey) && it.filter(unode.entries[i], wnode.entries[k])
                    it.isdone = false
                    break
                end

                # next w key
                if k < length(wnode)
                    k += 1
                else
                    k = 1
                    w = wnode.right

                    # When we hit a leaf node with no possible intersections, we
                    # start over from the top of the leaf. This is a heuristic
                    # to avoid linear search behavior in the presence of
                    # extremely long Segments.
                    if w !== nothing && w.maxend < first(ukey)
                        wnode = notnothing(w)
                        w, k = firstintersection(it.t2.root, ukey,
                                                 wnode.keys[wnode.count])
                    end
                end
            else
                break
            end
        end

        # intersection was found
        if !it.isdone
            break
        end

        # if no intersection found, advance u and start again
        @nextleafkey(unode, u, i)

        u === nothing && break

        # find u's first intersection
        unode = notnothing(u)
        ukey = unode.keys[i]

        w, k = firstintersection(it.t2.root, ukey, nothing)
    end

    it.u = u
    it.w = w
    it.i = i
    it.k = k
    return
end



# Intersect two Segment trees, returning an iterator yielding values of the
# form:
#   ((key1, value1), (key2, value2))
#
# Where key1 is from the first tree and key2 from the second, and they
# intersect.
function Base.intersect(t1::SegmentBTree{K, V1, B1},
                        t2::SegmentBTree{K, V2, B2},
                        filter::F=true_cmp; method=:auto) where {F, K, V1, B1, V2, B2}
    # We decide heuristically which intersection algorithm to use.
    m = length(t1)
    n = length(t2)

    if method == :auto
        iterative_cost  = n + m
        successive_cost = 0.25 * m * log(1 + n) + 1e5
        if iterative_cost < successive_cost
            return IntersectionIterator{F, K, V1, B1, V2, B2}(filter, t1, t2, false)
        else
            return IntersectionIterator{F, K, V1, B1, V2, B2}(filter, t1, t2, true)
        end
    elseif method == :successive
        return IntersectionIterator{F, K, V1, B1, V2, B2}(filter, t1, t2, true)
    elseif method == :iterative
        return IntersectionIterator{F, K, V1, B1, V2, B2}(filter, t1, t2, false)
    else
        error("No such intersection method: $method")
    end
end


