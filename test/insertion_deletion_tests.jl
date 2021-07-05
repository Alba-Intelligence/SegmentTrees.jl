

@testset "Insertion" begin
    t = SegmentMap{Int,Int}()
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
            k = (v, rand(v:maxend))
            t[k] = v
        end
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)
    end

    @testset "reverse ordered insertions" begin
        for v in 1:n
            k = (maxend - v, rand((maxend - v):maxend))
            t[k] = v
        end
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)
    end

    @testset "bulk insertion" begin
        segments = SegmentValue{Int,Int}[]
        for v in 1:n
            a, b = randsegment()
            push!(segments, SegmentValue{Int,Int}(a, b, v))
        end
        sort!(segments)
        t = SegmentMap{Int,Int}(segments)

        @test collect(keys(t)) == [Segment{Int}(segment.first, segment.last)
                                   for segment in segments]
        @test collect(values(t)) == [segment.value for segment in segments]
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)

        # don't break on empty arrays
        t = SegmentMap{Int,Int}(SegmentValue{Int,Int}[])
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)

        @test_throws MethodError SegmentTree{Int,Int}(segments, Int[1])
        shuffle!(segments)
        @test_throws UndefVarError SegmentTree{Int,Int}(segments, vals)
    end
end


@testset "Updates" begin
    t = SegmentMap{Int,Int}()
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
    t = SegmentBTree{Int,SegmentValue{Int,Int},8}()
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
    for segment in 1:(n - 1) / 2
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
    for i in mid + 1:length(segments)
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
    t = SegmentBTree{Int,SegmentValue{Int,Int},4}()
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
