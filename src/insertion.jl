
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


