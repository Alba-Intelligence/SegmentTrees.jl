
@testset "Segment Intersection" begin
    # generate n end-to-end segments
    t = SegmentMap{Int,Int}()
    segments = Any[]
    a = 1
    for i in 1:10000
        b = a + rand(0:100)
        t[(a, b)] = i
        push!(segments, (a, b))
        a = b + 1
    end

    # one
    x = rand(0:a - 1)
    @test length(collect(intersect(t, (x, x)))) === 1
    @test hasintersection(t, x)

    # nothing
    @test length(collect(intersect(t, (a, a)))) === 0
    @test !hasintersection(t, a)

    # everything
    @test length(collect(intersect(t, (segments[1][1], segments[end][2])))) == length(t)

    # always false predicate
    @test length(collect(intersect(t, (segments[1][1], segments[end][2]), (a, b) -> false))) == 0

    # half true
    @test length(collect(intersect(t, (segments[1][1], segments[end][2]), (a, b) -> isodd(a.value)))) == div(length(segments), 2)

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
    t = SegmentMap{Int,Int}()

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
    T = SegmentTrees.SegmentBTree{Int,Segment{Int},4}

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

    t = T([Segment(4, 4)])

    @test !(SegmentTrees.firstintersection(t.root, Segment(4, 4), Segment(2, 2)) === nothing)

    x = SegmentTrees.Intersection{Int,Segment{Int},4}()
    SegmentTrees.firstintersection!(t, Segment(4, 4), Segment(2, 2),
                                     x, SegmentTrees.true_cmp)
    @test x.index != 0
end


@testset "Nonunique" begin
    t = SegmentTree{Int,SegmentValue{Int,Int}}()
    n = 1000
    for i in 1:n
        push!(t, SegmentValue{Int,Int}(50, 100, i))
    end
    push!(t, SegmentValue{Int,Int}(1, 200, n + 1))
    push!(t, SegmentValue{Int,Int}(1, 1, n + 2))
    push!(t, SegmentValue{Int,Int}(200, 200, n + 3))

    @test (length(collect(t)) == length(t) == n + 3)
    @test issorted(collect(keys(t)))
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
end


@testset "Tree Intersection" begin
    n = 10000
    t1 = SegmentTrees.SegmentMap{Int,Int}()
    t2 = SegmentTrees.SegmentMap{Int,Int}()
    global maxend = 1000000
    for k in 1:n
        # generate small-ish segments so we avoid the worst case
        # of O(n^2) intersecting pairs and this test runs quickly
        u = rand(1:maxend)
        v = rand(u:u + 1000)
        t1[(u, v)] = k

        u = rand(1:maxend)
        v = rand(u:u + 1000)
        t2[(u, v)] = k
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
    @test length(collect(intersect(t1, t2, (a, b) -> a.value == b.value))) == count

    ## handle a particular intersection case that may not otherwise get hit
    t1 = SegmentMap{Int,Int}()
    t1[(1, 2)] = 1
    t2 = SegmentMap{Int,Int}()
    t2[(1001, 1002)] = 2
    @test isempty(collect(intersect(t1, t2, method=:iterative)))
    @test isempty(collect(intersect(t1, t2, method=:successive)))
    @test isempty(collect(intersect(t1, t2)))
end

