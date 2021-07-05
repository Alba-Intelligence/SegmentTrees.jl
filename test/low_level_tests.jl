
@testset "Slices" begin
    xs = Slice{Int,10}()

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
    V = SegmentValue{Int,Int}
    B = 32
    x = Segment{Int}(1, 1)
    node = InternalNode{K,V,B}()
    @test SegmentTrees.findidx(node, x) === 0
    @test SegmentTrees.firstfrom(node, 1) == (node, 0)
    node = LeafNode{K,V,B}()
    @test SegmentTrees.findidx(node, x) == 0

    result = SegmentTrees.Intersection{K,V,B}()
    SegmentTrees.firstintersection!(node, x, nothing, result, SegmentTrees.true_cmp)
    @test result.index == 0

    @test SegmentTrees.firstfrom(node, 1) == (node, 0)

    push!(node, SegmentValue{Int,Int}(1, 1, 1))
    @test SegmentTrees.firstfrom(node, 2) == (node, 0)

    # test that delete! still works on a contrived tree with one internal and
    # one leaf node
    t = SegmentBTree{K,V,B}()
    t.root = InternalNode{K,V,B}()
    push!(t.root.children, LeafNode{K,V,B}())
    push!(t.root.maxends, 1)
    push!(t.root.keys, x)
    push!(t.root.children[1], SegmentValue{Int,Int}(1, 1, 1))
    t.root.maxend = 1
    t.root.children[1].maxend = 1
    delete!(t, (1, 1))
    @test isa(t.root, LeafNode)

    ## test that the right thing happens if you delete the last key in a non-root
    ## leaf node (which can't actually happen)
    node = LeafNode{Int,SegmentValue{Int},32}()
    push!(node, SegmentValue{Int,Int}(1, 1, 1))
    @test SegmentTrees.findidx(node, x) == 1
end


