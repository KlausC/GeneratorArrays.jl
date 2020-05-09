module GeneratorArrays

export array

import Base.Generator, Base.Iterators.ProductIterator
import Base: IteratorSize, HasShape, HasLength, IteratorEltype, HasEltype
import Base: getindex, length, axes, size, iterate, eltype

"""
    GeneratorArray{T,N}(g) <: AbstractArray{T,N}

Wrapper of a `Base.Generator` or iterator `g` which provides reading access like an
abstract array.
The wrapped object must have a size `(IteratorSize(g) == HasShape())`.
"""
struct GeneratorArray{T,N,G} <: AbstractArray{T,N}
    gen::G
end

"""
    array(g::<generator or iterator>)

Construct a wrapped object of type `GeneratorArray(g)`.
The object provides the `AbstractArray` interface in, but does not support `setindex!`.

The wrapped object must have a size `(IteratorSize(g) == HasShape())`.
"""
array(a::AbstractArray) = a
array(gen::G) where G = _array(G)(gen)

# the AbstractArray interface implementation

getindex(a::GeneratorArray, i...) = _getindex(a.gen, CartesianIndex(i...)) 

iterate(a::GeneratorArray, s...) = iterate(a.gen, s...)

IteratorSize(::Type{<:GeneratorArray{T,N,G}}) where {T,N,G} = HasShape{N}()
length(a::GeneratorArray) = length(a.gen)
axes(a::GeneratorArray) = axes(a.gen)
size(a::GeneratorArray) = size(a.gen)

IteratorEltype(::Type{<:GeneratorArray{T,N,G}}) where {T,N,G} = HasEltype()
eltype(a::GeneratorArray{T}) where T = T

# implementation details

_array(::Type{G}) where G = _array(G, IteratorSize(G))
function _array(::Type{G}, ::HasShape) where G
    T = _eltype(G)
    N = dimension(G)
    GeneratorArray{T,N,G}
end
_array(T::Type, S::IteratorSize) = throw(DomainError(S, "unsupported iterator type $T"))

# the element type as inferred form the generator function - shortcut for identity
_eltype(itr) = eltype(itr)
function _eltype(gen::Type{<:Generator{B,F}}) where {B,F}
    E = _eltype(B)
    if F === typeof(identity)
        E
    else
        promote_type(Base.return_types(F.instance, (E,))...)
    end
end
_eltype(::Type{<:ProductIterator{T}}) where T = _prod_eltype(T)
_prod_eltype(::Type{Tuple{}}) = Tuple{}
function _prod_eltype(::Type{I}) where I<:Tuple
    head = Base.tuple_type_head(I)
    tail = Base.tuple_type_tail(I)
    Base.tuple_type_cons(_eltype(head), _prod_eltype(tail))
end

dimension(a::T) where T = dimension(T)
dimension(::Type{G}) where G = dimension(IteratorSize(G))
dimension(::IteratorSize) = 1
dimension(::HasShape{N}) where N = N

function _getindex(gen::G, i::CartesianIndex) where G<:Generator
    it = gen.iter
    N = dimension(G)
    i = adjust_indices(N, i, gen)
    arg = _getindex(it, i)
    gen.f(arg)
end

function _getindex(gen::G, i::CartesianIndex) where G<:ProductIterator
    it = gen.iterators
    N = dimension(G)
    n = length(it)
    i = adjust_indices(N, i, gen)
    res = Vector{Any}(undef, n)
    @inbounds for k = 1:n
        sit = it[k]
        di = dimension(sit)
        si, i = split(i, di)
        arg = _getindex(sit, si)
        res[k] = arg
    end
    tuple(res...)
end
_getindex(a, i::CartesianIndex) = getindex(a, i)

split(i::CartesianIndex, n::Integer) = Base.IteratorsMD.split(i, Val(n))

function adjust_indices(n::Integer, i::CartesianIndex, it)
    I = Tuple(i)
    m = length(I)
    m == n && return i
    (m == 0 || m > n && any(I[n+1:m] .!= 1)) && throw(BoundsError(it, I))  
    m < n ? CartesianIndex(I..., ones(Int, n-m)...) : CartesianIndex(I[1:n])
end

end # module
