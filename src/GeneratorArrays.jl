module GeneratorArrays

export array

import Base.Generator
import Base: IteratorSize, HasShape, HasLength, length, axes, size, iterate
import Base: IteratorEltype, eltype
import Base: getindex
import Base.Iterators: ProductIterator

struct GeneratorArray{T,N,G} <: AbstractArray{T,N}
    gen::G
end

array(a::AbstractArray) = a
array(gen::G) where G = _array(G)(gen)

function getindex(a::GeneratorArray{T,N,<:Generator}, i::Integer...) where {T,N}
    it = a.gen.iter
    i = adjust_indices(N, i, a)
    a.gen.f(array(it)[i...])::T
end

function getindex(a::GeneratorArray{T,N,<:ProductIterator}, i::Integer...) where {T,N}
    it = a.gen.iterators
    n = length(it)
    i = adjust_indices(N, i, it)
    dim = 0
    res = Vector{Any}(undef, n)
    for k = 1:n
        sit = it[k]
        di = dimension(sit) + dim
        si = i[dim+1:di]
        dim = di
        res[k] = getindex(array(sit), si...)
        #println(sit, "[", si..., "] = ", res[k])
    end
    tuple(res...)
end

iterate(a::GeneratorArray, s...) = iterate(a.gen, s...)

IteratorSize(::Type{<:GeneratorArray{T,N,G}}) where {T,N,G} = IteratorSize(G)
length(a::GeneratorArray) = length(a.gen)
axes(a::GeneratorArray) = axes(a.gen)
size(a::GeneratorArray) = size(a.gen)

IteratorEltype(::Type{<:GeneratorArray{T,N,G}}) where {T,N,G} = IteratorEltype(G)
eltype(a::GeneratorArray{T}) where T = T

# - implementation details

_array(::Type{G}) where G = _array(G, IteratorSize(G))
function _array(::Type{G}, ::HasShape) where G
    T = _eltype(G)
    N = dimension(G)
    GeneratorArray{T,N,G}
end
_array(T::Type, S::IteratorSize) = throw(DomainError(S, "unsupported iterator type $T"))

function _eltype(gen::Type{<:Generator{B,F}}) where {B,F}
    E = eltype(_array(B))
    if F === typeof(identity)
        E
    else
        promote_type(Base.return_types(F.instance, (E,))...)
    end
end
_eltype(::Type{<:ProductIterator{T}}) where T = _eltype(T)
_eltype(::Type{Tuple{I}}) where I = _eltype(I)
_eltype(::Type{Tuple{I,J}}) where {I,J} = Tuple{_eltype(I),_eltype(J)}
_eltype(::Type{Tuple{I,J,K}}) where {I,J,K} = Tuple{_eltype(I),_eltype(J),_eltype(K)}
_eltype(itr) = eltype(itr)

dimension(a::T) where T = dimension(T)
dimension(::Type{G}) where G = dimension(IteratorSize(G))
dimension(::IteratorSize) = 1
dimension(::HasShape{N}) where N = N

function adjust_indices(n::Integer, i, it)
    m = length(i)
    (m == 0 || m > n && any(i[n+1:m] .!= 1)) && throw(BoundsError(it, i))  
    m == n ? i : m < n ? (i..., ones(Int, n-m)...) : i[1:n]
end

end # module
