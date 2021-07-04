#
# Deleting
# --------



function deletefirst!(t::SegmentBTree{K, V, B}, first::K, last::K) where {K, V, B}
    return deletefirst!(t, Segment{K}(first, last))
end


function deletefirst!(t::SegmentBTree{K, V, B}, key::Tuple{K, K}) where {K, V, B}
    return deletefirst!(t, Segment{K}(key[1], key[2]))
end


function deletefirst!(t::SegmentBTree{K, V, B}, key::Segment{K}) where {K, V, B}
    result = _deletefirst!(t.root, key)
    if result.keyfound
        t.n -= 1
    end

    # if the root has only one child, promote the child
    if isa(t.root, InternalNode) && length(t.root) == 1
        t.root = t.root.children[1]
        t.root.parent = nothing
    end

    return t
end


# Indicate what steps are need to account for an updated child.
struct KeyFate
    value::UInt8
end

const KEYFATE_NONE         = KeyFate(0) # no changes
const KEYFATE_DELETE       = KeyFate(1) # delete the child node
const KEYFATE_DELETE_RIGHT = KeyFate(2) # delete the child's right sibling
const KEYFATE_UPDATE_LEFT  = KeyFate(3) # update the key separating the node
                                        # from its left sibling
const KEYFATE_UPDATE_RIGHT = KeyFate(4) # update the key separating the node
                                        # from its right sibling

# Information returned from a call to _delete
struct DeleteResult
    keyfound::Bool # true if the key was found

    # Indicate what steps are need to account for an updated child. One of:
    fate::KeyFate
end


# Delete key from the subtree if present. Return a DeleteResult object.
function _deletefirst!(t::InternalNode{K, V, B}, key::Segment{K}) where {K, V, B}
    i = findidx(t, key)
    j = i <= length(t) - 1 && key >= t.keys[i] ? i + 1 : i # child index

    ans = _deletefirst!(t.children[j], key)

    if !ans.keyfound
        return DeleteResult(false, KEYFATE_NONE)
    end

    if ans.fate == KEYFATE_UPDATE_LEFT
        t.maxend = nodemaxend(t)
        t.maxends[j] = t.children[j].maxend
        if j > 1
            t.maxends[j-1] = t.children[j-1].maxend
            t.keys[j-1] = minkey(t.children[j]);
            return DeleteResult(true, KEYFATE_NONE)
        else
            return DeleteResult(true, KEYFATE_UPDATE_LEFT)
        end
    elseif ans.fate == KEYFATE_UPDATE_RIGHT
        t.maxend = nodemaxend(t)
        t.maxends[j] = t.children[j].maxend
        if j < length(t)
            t.maxends[j+1] = t.children[j+1].maxend
            t.keys[j] = minkey(t.children[j+1])
        end

        if j > 1
            t.keys[j - 1] = minkey(t.children[j]);
            return DeleteResult(true, KEYFATE_NONE)
        else
            return DeleteResult(true, KEYFATE_UPDATE_LEFT)
        end
    elseif ans.fate == KEYFATE_DELETE || ans.fate == KEYFATE_DELETE_RIGHT
        deleteidx = ans.fate == KEYFATE_DELETE ? j : j + 1

        # deleteidx == 1 would only happen if we had only one child, but if
        # that were the case it would have been promoted.
        @assert deleteidx > 1

        splice!(t.keys, deleteidx - 1)
        splice!(t.children, deleteidx)
        splice!(t.maxends, deleteidx)
        t.maxend = nodemaxend(t)
        if ans.fate == KEYFATE_DELETE
            t.maxends[j-1] = t.children[j-1].maxend
        elseif ans.fate == KEYFATE_DELETE_RIGHT
            t.maxends[j] = t.children[j].maxend
        end

        # not underfull
        minsize = div(B, 2)
        if length(t) >= minsize
            return DeleteResult(true, deleteidx == 2 ? KEYFATE_UPDATE_LEFT : KEYFATE_NONE)
        end

        # borrow right
        if t.right !== nothing
            right = notnothing(t.right)
            if sameparent(right, t) && length(right) > minsize
                splice!(right.keys, 1)
                push!(t.children, splice!(right.children, 1))
                push!(t.maxends, splice!(right.maxends, 1))
                push!(t.keys, minkey(t.children[end]))
                t.maxend = max(t.maxend, nodemaxend(t.children[end]))
                right.maxend = nodemaxend(right)
                t.children[end].parent = t

                return DeleteResult(true, KEYFATE_UPDATE_RIGHT)
            end
        end

        # borrow left
        if t.left !== nothing
            left = notnothing(t.left)
            if sameparent(left, t) && length(left) > minsize
                insert!(t.children, 1, pop!(left.children))
                insert!(t.maxends, 1, pop!(left.maxends))
                pop!(left.keys)
                insert!(t.keys, 1, minkey(t.children[2]))
                t.maxend = max(t.maxend, nodemaxend(t.children[1]))
                left.maxend = nodemaxend(left)
                t.children[1].parent = t

                return DeleteResult(true, KEYFATE_UPDATE_LEFT)
            end
        end

        # merge with left
        if t.left !== nothing
            left = notnothing(t.left)
            if sameparent(left, t)
                merge!(left, t)
                left.right = t.right
                if t.right !== nothing
                    t.right = notnothing(t.right)
                    t.right.left = left
                end

                return DeleteResult(true, KEYFATE_DELETE)
            end
        end

        # merge with right
        if t.right !== nothing
            right = notnothing(t.right)
            if sameparent(right, t)
                merge!(t, right)
                if right.right !== nothing
                    right.right = notnothing(right.right)
                    right.right.left = t
                end
                t.right = right.right

                return DeleteResult(true, KEYFATE_DELETE_RIGHT)
            end
        end

        # Allow the root node to be underfull
        return DeleteResult(true, KEYFATE_NONE)
    else # KEYFATE_NONE
        t.maxend = nodemaxend(t)
        t.maxends[j] = t.children[j].maxend
        return DeleteResult(true, KEYFATE_NONE)
    end
end


function _deletefirst!(t::LeafNode{K, V, B}, key::Segment{K}) where {K, V, B}
    i = findidx(t, key)

    # do nothing if the key isn't present
    if i < 1 || i > length(t) ||
        first(t.keys[i]) != key.first || last(t.keys[i]) != key.last
        return DeleteResult(false, KEYFATE_NONE)
    end

    splice!(t, i)

    # This is the root node. Allow it to be empty.
    if isempty(t)
        return DeleteResult(true, KEYFATE_NONE)
    end

    minsize = div(B, 2)

    t.maxend = nodemaxend(t)

    # not underfull
    if length(t) >= minsize
        return DeleteResult(true, i == 1 ? KEYFATE_UPDATE_LEFT : KEYFATE_NONE)
    end

    # borrow right
    if t.right !== nothing
        right = notnothing(t.right)
        if sameparent(right, t) && length(right) > minsize
            push!(t, splice!(right, 1))
            t.maxend = max(t.maxend, last(t.keys[t.count]))
            right.maxend = nodemaxend(right)

            return DeleteResult(true, KEYFATE_UPDATE_RIGHT)
        end
    end

    # borrow left
    if t.left !== nothing
        left = notnothing(t.left)
        if sameparent(left, t) && length(left) > minsize
            insert!(t, 1, pop!(left))
            t.maxend = max(t.maxend, last(t.keys[1]))
            left.maxend = nodemaxend(left)

            return DeleteResult(true, KEYFATE_UPDATE_LEFT)
        end
    end

    # merge with left
    if t.left !== nothing
        left = notnothing(t.left)
        if sameparent(left, t)
            merge!(left, t)
            left.right = t.right
            if t.right !== nothing
                t.right = notnothing(t.right)
                t.right.left = left
            end
            return DeleteResult(true, KEYFATE_DELETE)
        end
    end

    # merge with right
    if t.right !== nothing
        right = notnothing(t.right)
        if sameparent(right, t)
            merge!(t, right)
            if right.right !== nothing
                right.right = notnothing(right.right)
                right.right.left = t
            end
            t.right = right.right
            return DeleteResult(true, KEYFATE_DELETE_RIGHT)
        end
    end

    # This must be the root node. Allow it to be underfull.
    return DeleteResult(true, KEYFATE_NONE)
end
