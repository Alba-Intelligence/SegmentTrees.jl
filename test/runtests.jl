#!/usr/bin/env julia

using SegmentTrees
import SegmentTrees: Slice, InternalNode, LeafNode, Segment, SegmentBTree
using Test
using Random: seed!, shuffle!


@testset "All tests" begin
    include("creation_tests.jl")
    include("insertion_deletion_tests.jl")
    include("intersection_tests.jl")
    include("search_tests.jl")
    include("iteration.jl")
    include("low_level_tests.jl")
end
