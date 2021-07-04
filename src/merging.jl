
# Join two leaf nodes into one.
function merge!(left::LeafNode{K, V, B}, right::LeafNode{K, V, B}) where {K, V, B}
    leftlen, rightlen = length(left), length(right)
    @assert leftlen + rightlen <= B
    resize!(left, leftlen + rightlen)
    copyto!(left.entries, leftlen+1, right.entries, 1, length(right))
    copyto!(left.keys, leftlen+1, right.keys, 1, length(right))
    left.maxend = nodemaxend(left)
    return
end


# Join two internal nodes into one
function merge!(left::InternalNode{K, V, B}, right::InternalNode{K, V, B}) where {K, V, B}
    leftlen, rightlen = length(left), length(right)
    @assert length(left) + length(right) <= B
    resize!(left.keys, leftlen + rightlen - 1)
    resize!(left.children, leftlen + rightlen)
    resize!(left.maxends, leftlen + rightlen)
    copyto!(left.children, leftlen+1, right.children, 1, length(right.children))
    copyto!(left.maxends, leftlen+1, right.maxends, 1, length(right.maxends))
    for i in leftlen+1:length(left.children)
        left.children[i].parent = left
    end
    left.keys[leftlen] = minkey(left.children[leftlen+1])
    copyto!(left.keys, leftlen+1, right.keys, 1, length(right.keys))
    left.maxend = nodemaxend(left)
    return
end

