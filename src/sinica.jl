

"""
Types deriving from `AbstractSegment{T}` must have a `range[1]` and `range[2]`
function each returning a value of type `T`, and `first(i) <= last(i)`
must always be true.
"""
abstract type AbstractSegment{T} end

Base.first(i::AbstractSegment{T}) where T = i.range[1]
Base.last(i::AbstractSegment{T}) where T = i.range[1]

function basic_isless(u::AbstractSegment, v::AbstractSegment)
    return first(u) < first(v) || (first(u) == first(v) && last(u) < last(v))
end

function Base.isless(u::AbstractSegment, v::AbstractSegment)
    return basic_isless(u, v)
end

"""
A basic Segment.
"""
mutable struct Segment{T} <: AbstractSegment{T}
    range::Tuple{Int,Int}
    aux::Union{Nothing,T}

    left::Segment{T}
    right::Segment{T}

    function Segment(s::Int, Int::T, aux::T) where {T}
        @assert range[1] < range[2] "The beginning of the segment must be lower than the end."

        key = nothing
        if range[2] - range[1] > 1
            key = ceil((range[1] + range[2]) / 2)
        end

        return new((s, t) , aux, Segment((s, key), nothing), Segment((key, t), nothing))
    end 
    
end


"""
    DataArray
"""
DataArray{T} = Vector{T}


