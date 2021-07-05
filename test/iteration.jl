

@testset "Iteration" begin
    t = SegmentMap{Int,Int}()
    @test isempty([x for x in t])

    @testset "from" begin
        n = 100
        global maxend = 1000000
        segments = [randsegment(1, maxend) for _ in 1:n]
        startpos = 50000
        expected_count = 0
        t = SegmentMap{Int,Int}()
        for (i, segment) in enumerate(segments)
            t[segment] = i
            if segment[2] >= startpos
                expected_count += 1
            end
        end

        @test length(collect(from(t, startpos))) === expected_count
        @test length(collect(from(t, 0))) === n
        @test length(collect(from(t, maxend + 1))) === 0
        @test length(collect(from(SegmentTree{Int,Int}(), 0))) === 0
    end
end

