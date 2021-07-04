#!/usr/bin/env julia

using SegmentTrees
import SegmentTrees: Slice, InternalNode, LeafNode, Segment, SegmentBTree
using Test
using Random: seed!, shuffle!


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


@testset "Search" begin
    t = SegmentMap{Int, Int}()
    @test !haskey(t, (1,2))

    n = 10000
    global maxend = 1000000
    segments = [randsegment(1, maxend) for i in 1:n]

    @testset "true positives" begin
        for (i, segment) in enumerate(segments)
            t[segment] = i
        end

        shuffle!(segments)
        @test all(Bool[haskey(t, segment) for segment in segments])
        @test all(Bool[findfirst(t, segment) !== nothing for segment in segments])

        results = Bool[]
        for segment in segments
            x = findfirst(t, segment)
            push!(results, x.first == segment[1] && x.last == segment[2])
        end
        @test all(results)

        @test all(Bool[(get!(t, segment, -1) != SegmentValue{Int, Int}(segment[1], segment[2], -1))
                       for segment in segments])
        @test all(Bool[get(t, segment, -1) != -1 for segment in segments])
    end

    @testset "true negatives" begin
        @test all(Bool[!haskey(t, segment)
                       for segment in [randsegment(maxend+1, 2 * maxend)
                                        for i in 1:n]])
        @test all(Bool[(findfirst(t, segment) === nothing)
                       for segment in [randsegment(maxend+1, 2 * maxend)
                                        for i in 1:n]])

        @test all(Bool[(findfirst(t, segment, (a,b)->false) === nothing) for segment in segments])

        @test all(Bool[get!(t, segment, -1) == SegmentValue{Int, Int}(segment[1], segment[2], -1)
                       for segment in [randsegment(maxend+1, 2 * maxend)
                                        for i in 1:n]])
        @test all(Bool[get(t, segment, -1) == -1
                       for segment in [randsegment(maxend+1, 2 * maxend)
                                        for i in 1:n]])
    end
end


@testset "Iteration" begin
    t = SegmentMap{Int, Int}()
    @test isempty([x for x in t])

    @testset "from" begin
        n = 100
        global maxend = 1000000
        segments = [randsegment(1, maxend) for _ in 1:n]
        startpos = 50000
        expected_count = 0
        t = SegmentMap{Int, Int}()
        for (i, segment) in enumerate(segments)
            t[segment] = i
            if segment[2] >= startpos
                expected_count += 1
            end
        end

        @test length(collect(from(t, startpos))) === expected_count
        @test length(collect(from(t, 0))) === n
        @test length(collect(from(t, maxend + 1))) === 0
        @test length(collect(from(SegmentTree{Int, Int}(), 0))) === 0
    end
end


@testset "Segment Intersection" begin
    # generate n end-to-end segments
    t = SegmentMap{Int, Int}()
    segments = Any[]
    a = 1
    for i in 1:10000
        b = a + rand(0:100)
        t[(a, b)] = i
        push!(segments, (a, b))
        a = b + 1
    end

    # one
    x = rand(0:a-1)
    @test length(collect(intersect(t, (x, x)))) === 1
    @test hasintersection(t, x)

    # nothing
    @test length(collect(intersect(t, (a, a)))) === 0
    @test !hasintersection(t, a)

    # everything
    @test length(collect(intersect(t, (segments[1][1], segments[end][2])))) == length(t)

    # always false predicate
    @test length(collect(intersect(t, (segments[1][1], segments[end][2]), (a,b)->false))) == 0

    # half true
    @test length(collect(intersect(t, (segments[1][1], segments[end][2]), (a,b)->isodd(a.value)))) == div(length(segments), 2)

    @test all(Bool[hasintersection(t, segment[1]) for segment in segments])
    @test all(Bool[hasintersection(t, segment[end]) for segment in segments])

    # some random intersection queries
    function random_intersection_query()
        i = rand(1:length(segments))
        j = rand(i:length(segments))
        return length(collect(intersect(t, (segments[i][1], segments[j][2])))) == j - i + 1
    end

    @test all(Bool[random_intersection_query() for i in 1:1000])

    # segments separated by 1
    t = SegmentMap{Int, Int}()

    @test !hasintersection(t, 1)

    segments = Any[]
    gaps = Any[]
    a = 1
    for i in 1:10000
        b = a + rand(0:100)
        if iseven(b)
            t[a, b] = i
            push!(segments, (a, b))
        else
            push!(gaps, (a, b))
        end
        a = b + 1
    end

    @test all(Bool[hasintersection(t, segment[1]) for segment in segments])
    @test all(Bool[hasintersection(t, segment[end]) for segment in segments])
    @test !all(Bool[hasintersection(t, segment[1]) for segment in gaps])
    @test !all(Bool[hasintersection(t, segment[end]) for segment in gaps])

    # firstintersection with lower bound
    T = SegmentTrees.SegmentBTree{Int, Segment{Int}, 4}

    xs = [
        # leaf node 1
        Segment(1, 1),
        Segment(1, 4),
        Segment(2, 2),
        Segment(2, 2),

        # leaf node 2
        Segment(3, 3),
        Segment(3, 3),
        Segment(3, 3),
        Segment(3, 3),

        # leaf node 3
        Segment(4, 4),
        Segment(4, 4),
        Segment(4, 4),
        Segment(4, 4),
    ]

    t = T([Segment(4,4)])

    @test !(SegmentTrees.firstintersection(t.root, Segment(4,4), Segment(2,2)) === nothing)

    x = SegmentTrees.Intersection{Int, Segment{Int}, 4}()
    SegmentTrees.firstintersection!(t, Segment(4,4), Segment(2,2),
                                     x, SegmentTrees.true_cmp)
    @test x.index != 0
end


@testset "Nonunique" begin
    t = SegmentTree{Int, SegmentValue{Int, Int}}()
    n = 1000
    for i in 1:n
        push!(t, SegmentValue{Int, Int}(50, 100, i))
    end
    push!(t, SegmentValue{Int, Int}(1, 200, n + 1))
    push!(t, SegmentValue{Int, Int}(1, 1, n + 2))
    push!(t, SegmentValue{Int, Int}(200, 200, n + 3))

    @test (length(collect(t)) == length(t) == n + 3)
    @test issorted(collect(keys(t)))
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
end


@testset "Tree Intersection" begin
    n = 10000
    t1 = SegmentTrees.SegmentMap{Int, Int}()
    t2 = SegmentTrees.SegmentMap{Int, Int}()
    global maxend = 1000000
    for k in 1:n
        # generate small-ish segments so we avoid the worst case
        # of O(n^2) intersecting pairs and this test runs quickly
        u = rand(1:maxend)
        v = rand(u:u+1000)
        t1[(u,v)] = k

        u = rand(1:maxend)
        v = rand(u:u+1000)
        t2[(u,v)] = k
    end

    # Since SegmentTrees has two tree intersection algorithms, I'm
    # testing by checking that they are in agreement.
    a = intersect(t1, t2, method=:successive)
    b = intersect(t1, t2, method=:iterative)
    @test collect(a) == collect(b)
    @test length(collect(a)) == length(collect(b)) > 0

    # test filter predicate
    count = 0
    for (a, b) in intersect(t1, t2)
        if a.value == b.value
            count += 1
        end
    end
    @test length(collect(intersect(t1, t2, (a,b) -> a.value == b.value))) == count

    ## handle a particular intersection case that may not otherwise get hit
    t1 = SegmentMap{Int, Int}()
    t1[(1, 2)] = 1
    t2 = SegmentMap{Int, Int}()
    t2[(1001, 1002)] = 2
    @test isempty(collect(intersect(t1, t2, method=:iterative)))
    @test isempty(collect(intersect(t1, t2, method=:successive)))
    @test isempty(collect(intersect(t1, t2)))
end


@testset "Insertion" begin
    t = SegmentMap{Int, Int}()
    n = 100000

    @testset "random insertions" begin
        for v in 1:n
            k = randsegment()
            t[k] = v
        end
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)
    end

    @testset "ordered insertions" begin
        for v in 1:n
            k = (v,rand(v:maxend))
            t[k] = v
        end
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)
    end

    @testset "reverse ordered insertions" begin
        for v in 1:n
            k = (maxend-v,rand((maxend-v):maxend))
            t[k] = v
        end
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)
    end

    @testset "bulk insertion" begin
        segments = SegmentValue{Int, Int}[]
        for v in 1:n
            a, b = randsegment()
            push!(segments, SegmentValue{Int, Int}(a, b, v))
        end
        sort!(segments)
        t = SegmentMap{Int, Int}(segments)

        @test collect(keys(t)) == [Segment{Int}(segment.first, segment.last)
                                   for segment in segments]
        @test collect(values(t)) == [segment.value for segment in segments]
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)

        # don't break on empty arrays
        t = SegmentMap{Int, Int}(SegmentValue{Int, Int}[])
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)

        @test_throws MethodError SegmentTree{Int, Int}(segments, Int[1])
        shuffle!(segments)
        @test_throws UndefVarError SegmentTree{Int, Int}(segments, vals)
    end
end


@testset "Updates" begin
    t = SegmentMap{Int, Int}()
    n = 100000
    ks = [randsegment() for _ in 1:n]
    for (v, k) in enumerate(ks)
        t[k] = v
    end

    shuffle!(ks)
    for (v, k) in enumerate(ks)
        t[k] = v
    end

    @test issorted(collect(keys(t)))
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
end


@testset "Deletion" begin
    # Set B to smaller number to try to induce more weird merges and borrows and
    # such
    t = SegmentBTree{Int, SegmentValue{Int, Int}, 8}()
    n = 100000

    # insert n random segments
    segments = [randsegment() for _ in 1:n]
    for (i, segment) in enumerate(segments)
        t[segment] = i
    end
    shuffle!(segments)

    # delete non-existant is a nop
    @test delete!(t, (-1, -1)).n === n

    # delete one
    segment = pop!(segments)
    delete!(t, segment)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
    @test issorted(collect(keys(t)))
    @test !haskey(t, segment)
    @test all(segment -> haskey(t, segment), segments)

    # delete a random 50%
    for segment in 1:(n-1)/2
        segment = pop!(segments)
        delete!(t, segment)
    end
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
    @test issorted(collect(keys(t)))
    @test all(segment -> haskey(t, segment), segments)

    # delete the rest
    for segment in segments
        delete!(t, segment)
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)

    # delete left-to-right
    shuffle!(segments)
    for (i, segment) in enumerate(segments)
        t[segment] = i
    end
    sort!(segments)
    for segment in segments
        delete!(t, segment)
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)

    # delete right-to-left
    shuffle!(segments)
    for (i, segment) in enumerate(segments)
        t[segment] = i
    end
    reverse!(segments)
    for segment in segments
        delete!(t, segment)
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)

    # delete from the middle
    shuffle!(segments)
    for (i, segment) in enumerate(segments)
        t[segment] = i
    end
    mid = div(length(segments), 2)
    for i in mid+1:length(segments)
        delete!(t, segments[i])
    end
    for i in mid:-1:1
        delete!(t, segments[i])
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)

    # contrive a special case: merge with the left when we have a non-null
    # right
    t = SegmentBTree{Int, SegmentValue{Int, Int}, 4}()
    for i in 1:24
        t[(i, i)] = i
    end
    @test isa(t.root, InternalNode)
    @test length(t.root.children) === 3
    @test length(t.root.children[2].children) === 2
    delete!(t, (15, 15))
    keys_to_delete = collect(keys(t))
    for key in keys_to_delete
        delete!(t, key)
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
end


@testset "Slices" begin
    xs = Slice{Int, 10}()

    @test_throws BoundsError xs[0]
    @test_throws BoundsError xs[11]
    @test_throws BoundsError xs[0] = 1
    @test_throws BoundsError xs[11] = 1
    @test_throws BoundsError pop!(xs)
    @test_throws BoundsError insert!(xs, 0, 1)

    for x in 1:10
        push!(xs, x)
    end

    @test_throws BoundsError push!(xs, 11)
    @test_throws BoundsError insert!(xs, 11, 1)
    @test_throws BoundsError resize!(xs, 11)
    @test_throws BoundsError splice!(xs, 0)
    @test_throws BoundsError splice!(xs, 11)
end


# Abuse low-level functions to test cases that wouldn't otherwise occur
@testset "Low Level" begin
    # findidx should return 0 when called on an empty node
    K = Int
    V = SegmentValue{Int, Int}
    B = 32
    x = Segment{Int}(1, 1)
    node = InternalNode{K, V, B}()
    @test SegmentTrees.findidx(node, x) === 0
    @test SegmentTrees.firstfrom(node, 1) == (node, 0)
    node = LeafNode{K, V, B}()
    @test SegmentTrees.findidx(node, x) == 0

    result = SegmentTrees.Intersection{K, V, B}()
    SegmentTrees.firstintersection!(node, x, nothing, result, SegmentTrees.true_cmp)
    @test result.index == 0

    @test SegmentTrees.firstfrom(node, 1) == (node, 0)

    push!(node, SegmentValue{Int, Int}(1, 1, 1))
    @test SegmentTrees.firstfrom(node, 2) == (node, 0)

    # test that delete! still works on a contrived tree with one internal and
    # one leaf node
    t = SegmentBTree{K, V, B}()
    t.root = InternalNode{K, V, B}()
    push!(t.root.children, LeafNode{K, V, B}())
    push!(t.root.maxends, 1)
    push!(t.root.keys, x)
    push!(t.root.children[1], SegmentValue{Int, Int}(1, 1, 1))
    t.root.maxend = 1
    t.root.children[1].maxend = 1
    delete!(t, (1,1))
    @test isa(t.root, LeafNode)

    ## test that the right thing happens if you delete the last key in a non-root
    ## leaf node (which can't actually happen)
    node = LeafNode{Int, SegmentValue{Int}, 32}()
    push!(node, SegmentValue{Int, Int}(1, 1, 1))
    @test SegmentTrees.findidx(node, x) == 1
end
