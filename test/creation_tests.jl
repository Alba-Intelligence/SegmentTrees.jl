
# Convert
@testset "Convert and constructors" begin
    @test Segment(1:5) == Segment(1, 5)
    @test Segment(4.2:9.2) == Segment(4.2, 9.2)
    @test first(SegmentValue(1:5, "Hi!")) == 1
    @test last(SegmentValue(1:5, "Hi!")) == 5
    @test value(SegmentValue(1:5, "Hi!")) == "Hi!"
    @test first(SegmentValue(4.2:9.2, "Bye!")) == 4.2
    @test last(SegmentValue(4.2:9.2, "Bye!")) == 9.2
    @test value(SegmentValue(4.2:9.2, "Bye!")) == "Bye!"
end

# Getters
@testset "Getters" begin
    i = Segment(5, 6)
    iv = SegmentValue(10, 11, "FOO")
    @test first(i)  === 5
    @test last(i)   === 6
    @test first(iv) === 10
    @test last(iv)  === 11
    @test value(iv) == "FOO"
end

# Generating random Segments
seed!(12345)
global maxend = round(Int, 1e9)

function randsegment(minstart=1, maxend=maxend)
    a = rand(minstart:maxend)
    b = rand(a:maxend)
    return (a, b)
end


# Verify that internal node keys and maxend values are correct
function validkeys(t::SegmentTrees.SegmentBTree{K, V, B}) where {K, V, B}
    minint = SegmentTrees.Segment{K}(0, 0)
    maxint = SegmentTrees.Segment{K}(maxend, maxend)
    return validkeys(t.root, minint, maxint)
end


function validkeys(node::SegmentTrees.InternalNode, minint, maxint)
    for (i, child) in enumerate(node.children)
        if child.maxend > node.maxend
            return false
        end

        if child.maxend != node.maxends[i]
            return false
        end
    end

    for i in 1:length(node.keys)
        k = node.keys[i]
        if SegmentTrees.minkey(node.children[i+1]) != k
            return false
        end

        if !validkeys(node.children[i], minint, k) ||
           !validkeys(node.children[i+1], k, maxint)
           return false
       end
    end
    return true
end


function validkeys(node::SegmentTrees.LeafNode, minint, maxint)
    for (key, entry) in zip(node.keys[1:node.count], node.entries[1:node.count])
        if first(key) != first(entry) || last(key) != last(entry)
            return false
        end

        segment = SegmentTrees.Segment(first(entry), last(entry))

        if !(minint <= segment <= maxint) || last(segment) > node.maxend
            return false
        end
    end
    return true
end


# Verify that parent pointers are correct
function validparents(t::SegmentTrees.SegmentBTree)
    if (t.root.parent !== nothing)
        return false
    end

    return validparents(t.root)
end


function validparents(node::SegmentTrees.InternalNode)
    for child in node.children
        if !validparents(child) || child.parent != node
            return false
        end
    end
    return true
end

validparents(node::SegmentTrees.LeafNode) = true

# Verify that sibling/cousin pointers are correct
function validsiblings(t::SegmentTrees.SegmentBTree)
    t.root.left === t.root.right === nothing || return false

    # Do an in-order traversal, pushing nodes onto a stack indexed by
    # depth.
    tdepth = depth(t)
    nodestacks = [Vector{SegmentTrees.Node}(undef, 0) for _ in 1:tdepth]

    function _visit(node::SegmentTrees.InternalNode, k)
        push!(nodestacks[k], node)
        for child in node.children
            _visit(child, k+1)
        end
    end

    function _visit(node::SegmentTrees.LeafNode, k)
        push!(nodestacks[k], node)
    end

    _visit(t.root, 1)

    for k in 1:tdepth
        stack = nodestacks[k]
        if isempty(stack)
            continue
        end

        if !(stack[1].left === nothing) || !(stack[end].right === nothing)
            return false
        end

        if length(stack) > 1
            if (stack[1].right === nothing)  || stack[1].right != stack[2] ||
               (stack[end].left === nothing) || stack[end].left != stack[end-1]
                return false
            end
        end

        for i in 2:length(stack)-1
            if (stack[i].left === nothing)  || stack[i].left != stack[i-1] ||
               (stack[i].right === nothing) || stack[i].right != stack[i+1]
                return false
            end
        end
    end

    return true
end

