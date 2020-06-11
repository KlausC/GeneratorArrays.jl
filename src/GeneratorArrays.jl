module GeneratorArrays

export array

using Base.Iterators

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
array(a::AbstractString) = [x for x in a]
array(gen) = _array(gen)

# the AbstractArray interface implementation

getindex(a::GeneratorArray, i::Integer...) = _getindex(a.gen, CartesianIndex(i...)) 

iterate(a::GeneratorArray, s...) = iterate(a.gen, s...)

IteratorSize(::Type{<:GeneratorArray{T,N,G}}) where {T,N,G} = HasShape{N}()
length(a::GeneratorArray) = length(a.gen)
axes(a::GeneratorArray) = axes(a, IteratorSize(a.gen))
size(a::GeneratorArray) = size(a, IteratorSize(a.gen))
axes(a::GeneratorArray, ::HasShape) = axes(a.gen)
size(a::GeneratorArray, ::HasShape) = size(a.gen)
axes(a::GeneratorArray, ::HasLength) = (Base.OneTo(length(a.gen)),)
size(a::GeneratorArray, ::HasLength) = (length(a.gen),)

IteratorEltype(::Type{<:GeneratorArray{T,N,G}}) where {T,N,G} = HasEltype()
eltype(a::GeneratorArray{T}) where T = T

# implementation details

_array(gen::G) where G = _array(gen, IteratorSize(G))
function _array(gen::G, ::Union{HasShape,HasLength}) where G<:Generator{<:Any,typeof(identity)}
    _array(gen.iter)
end
function _array(gen::G, ::Union{HasShape,HasLength}) where G
    T = _eltype(gen)
    N = dimension(G)
    GeneratorArray{T,N,G}(gen)
end
_array(::T, S::IteratorSize) where T = throw(DomainError(S, "unsupported iterator type $T"))

# the element type as inferred form the generator function - shortcut for identity
_eltype(gen::G) where G = eltype(G)
function _eltype(gen::Generator{B,F}) where {B,F}
    E = _eltype(gen.iter)
    f = gen.f
    if f === identity
        E
    elseif isempty(gen.iter)
        try
            typeof(fnothings(f, E))
        catch
            Base.Bottom
        end
    else
        typeof(f(first(gen.iter)))
    end
end
_eltype(gen::ProductIterator{T}) where T = Base.to_tuple_type(_eltype.(gen.iterators))

fnothings(f, E::Tuple) = f(ntuple(x->nothing, length(E))...)
fnothings(f, ::Type) = f(nothing)

dimension(a::T) where T = dimension(T)
dimension(::Type{G}) where G = dimension(IteratorSize(G))
dimension(::IteratorSize) = 1
dimension(::HasShape{N}) where N = N

function _getindex(gen::G, i::CartesianIndex) where G<:Generator
    it = gen.iter
    N = dimension(G)
    i = adjust_indices(Val(N), i, gen)
    arg = _getindex(it, i)
    gen.f(arg)
end

function _getindex(gen::G, i::CartesianIndex) where G<:ProductIterator
    it = gen.iterators
    N = dimension(G)
    n = length(it)
    i = adjust_indices(Val(N), i, gen)
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

function _getindex(gen::G, i::CartesianIndex) where G<:Iterators.Take{<:AbstractArray}
    getindex(gen.xs[i])
end
function _getindex(gen::G, i::CartesianIndex{N}) where {N,G<:Iterators.Drop{<:AbstractArray}}
    offset = CartesianIndex(ntuple(i -> i == 1 ? gen.n : 0, N))
    getindex(gen.xs[i+offset])
end

_getindex(a::AbstractArray, i::CartesianIndex) = getindex(a, i)

split(i::CartesianIndex, n::Integer) = Base.IteratorsMD.split(i, Val(n))

function adjust_indices(n::Val{N}, i::CartesianIndex{M}, it) where {N,M}
    M == N && return i
    I = i.I
    (M == 0 || M > N && any(I[N+1:M] .!= 1)) && throw(BoundsError(it, I))  
    CartesianIndex(ntuple(j -> j <= M ? I[j] : 1, n))
end

end # module
