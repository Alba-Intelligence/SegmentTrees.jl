
@testset "Search" begin
    t = SegmentMap{Int,Int}()
    @test !haskey(t, (1, 2))

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

        @test all(Bool[(get!(t, segment, -1) != SegmentValue{Int,Int}(segment[1], segment[2], -1))
                       for segment in segments])
        @test all(Bool[get(t, segment, -1) != -1 for segment in segments])
    end

    @testset "true negatives" begin
        @test all(Bool[!haskey(t, segment)
                       for segment in [randsegment(maxend + 1, 2 * maxend)
                                        for i in 1:n]])
        @test all(Bool[(findfirst(t, segment) === nothing)
                       for segment in [randsegment(maxend + 1, 2 * maxend)
                                        for i in 1:n]])

        @test all(Bool[(findfirst(t, segment, (a, b) -> false) === nothing) for segment in segments])

        @test all(Bool[get!(t, segment, -1) == SegmentValue{Int,Int}(segment[1], segment[2], -1)
                       for segment in [randsegment(maxend + 1, 2 * maxend)
                                        for i in 1:n]])
        @test all(Bool[get(t, segment, -1) == -1
                       for segment in [randsegment(maxend + 1, 2 * maxend)
                                        for i in 1:n]])
    end
end
