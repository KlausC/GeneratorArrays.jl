module GeneratorArrays

export array, GeneratorArray

import Base.Generator
import Base: IteratorSize, HasShape, HasLength, length, axes
import Base: IteratorEltype, eltype
import Base: getindex, iterate
import Base.Iterators: ProductIterator

struct GeneratorArray{T,N,G} <: AbstractArray{T,N}
    gen::G
end

array(a::AbstractArray) = a

function array(gen::G) where G
    array(G)(gen)
end

array(::Type{G}) where G = array(G, IteratorSize(G))

function array(::Type{G}, ::HasShape) where G
    T = _eltype(G)
    N = dimension(G)
    GeneratorArray{T,N,G}
end

IteratorSize(::Type{<:GeneratorArray{T,N,G}}) where {T,N,G} = IteratorSize(G)
length(a::GeneratorArray) = length(a.gen)
axes(a::GeneratorArray) = axes(a.gen)

IteratorEltype(::Type{<:GeneratorArray{T,N,G}}) where {T,N,G} = IteratorEltype(G)
eltype(a::GeneratorArray{T}) where T = T

function _eltype(gen::Type{<:Generator{B,F}}) where {B,F}
    E = eltype(array(B))
    if F === typeof(identity)
        E
    else
        promote_type(Base.return_types(F.instance, (E,))...)
    end
end

_eltype(itr) = eltype(itr)

dimension(a::T) where T = dimension(T)
dimension(::Type{G}) where G = dimension(IteratorSize(G))
dimension(::IteratorSize) = 1
dimension(::HasShape{N}) where N = N

function getindex(a::GeneratorArray{T,N,<:Generator}, i::Integer...) where {T,N}
    it = a.gen.iter
    i = adjust_indices(N, i)
    a.gen.f(array(it)[i...])::T
end

function getindex(a::GeneratorArray{T,N,<:ProductIterator}, i::Integer...) where {T,N}
    it = a.gen.iterators
    i = adjust_indices(length(it), i)
    getindex.(it, i)::T
end

function adjust_indices(n::Integer, i)
    m = n - length(i)
    m == 0 ? i : m > 0 ? (i..., ones(Int, m)...) : i[1:n]
end

iterate(a::GeneratorArray, s...) = iterate(a.gen, s...)

end # module
