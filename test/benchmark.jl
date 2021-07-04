if VERSION > v"0.7-"
    push!(LOAD_PATH, Base.NamedEnv("SegmentTrees.jl"))
    using InteractiveUtils
    using Random
    using Profile
end
versioninfo()

using SegmentTrees
using CodecZlib

function readgff3(input::IO, chrom::String)
    segments = Segment{Int}[]
    for line in eachline(input)
        if startswith(line, "#")
            continue
        end
        values = split(line, '\t')
        if values[1] == chrom
            chromstart = parse(Int, values[4])
            chromend = parse(Int, values[5])
            push!(segments, Segment(chromstart, chromend))
        end
    end
    sort!(segments)
    return SegmentTree{Int,Segment{Int}}(segments)
end

function readgff3(filepath::String, chrom::String)
    stream = open(filepath)
    if endswith(filepath, ".gz")
        stream = GzipDecompressorStream(stream)
    end
    try
        return readgff3(stream, chrom)
    finally
        close(stream)
    end
end

isdir("data") || mkdir("data")
cd("data")
filename="Danio_rerio.GRCz11.92.gff3.gz"
isfile(filename)||
    download("ftp://ftp.ensembl.org/pub/release-92/gff3/danio_rerio/Danio_rerio.GRCz11.92.gff3.gz", filename)
cd("..")

tree = readgff3("data/Danio_rerio.GRCz11.92.gff3.gz", "1")

println("random query")
function random_query(tree, queries)
    n = 0
    for q in queries
        for _ in intersect(tree, q)
            n += 1
        end
    end
    return n
end
srand(1234)
segments = shuffle!(collect(tree))
for _ in 1:5
    @time random_query(tree, segments)
end
if "-p" in ARGS
    @profile random_query(tree, segments)
    Profile.print()
end
