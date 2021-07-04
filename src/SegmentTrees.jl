module SegmentTrees

export
    SegmentTree,
    SegmentMap,
    AbstractSegment,
    Segment,
    SegmentValue,
    depth,
    hasintersection,
    from,
    value

using Base: notnothing

include("slice.jl")
include("types.jl")
include("deletion.jl")
include("merging.jl")
include("searching.jl")
include("intersection.jl")
include("map.jl")


# Diagnostics
# -----------


# Dumb tree printing, useful only for debuging.
#function showtree(io::IO, t::SegmentBTree)
    #showtree(io, t.root, 0)
#end


#function showtree(t::SegmentBTree)
    #showtree(STDOUT, t)
#end


#function showtree(io::IO, t::InternalNode, indent::Int)
    #for (i, child) in enumerate(t.children)
        #showtree(io, child, indent+1)
        #if i <= length(t.keys)
            #print(io, repeat("  ", indent), t.keys[i], ":\n")
        #end
    #end
#end


#function showtree(io::IO, t::LeafNode, indent::Int)
    #for (k, v) in zip(t.keys, t.values)
        #print(io, repeat("  ", indent), k, ": ", v, "\n")
    #end
#end


end  # module SegmentTrees
