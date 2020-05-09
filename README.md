# GeneratorArrays

[![Travis Build Status](https://travis-ci.org/KlausC/GeneratorArrays.jl.svg?branch=master)](https://travis-ci.org/KlausC/GeneratorArrays.jl)
[![codecov.io](http://codecov.io/github/KlausC/GeneratorArrays.jl/coverage.svg?branch=master)](http://codecov.io/github/KlausC/GeneratorArrays.jl?branch=master)

## Array view to Generators and Iterators

Effectively provide an `AbstractArray` view of a generator or iterator.

That means it will be possible to pass a generator to methods expecting
arrays. The only exported function is called `array` and `array(g)` can be
used like a read-only abstract array.

The restrictions to the generators and iterators are that they need to have
a `size`.

Usage:
```juliadoc

    using GeneratorArrays

    a = array(x^2 for x in 0:0.01:1)

    a[1] == 0
    a[51] ≈ 0.25
    a[101] == 1


```

For example you can exploit the binary-search functionality of 
`searchsorted` without evaluating the function at all grid points.

```
    using GeneratorArrays

    n = 10^6
    a = randn(ComplexF64, n);
    p = sortperm(a, by=real);

    # positions in a[p] where numbers in corresponding vertical stripes are found
    res = Int[]
    for h in -3:0.1:3
        x = searchsortedfirst(array(real(a[p[i]]) for i in 1:n), h)
        push!(res, x)
    end
    
    mysin(x) = begin println(x); sin(x); end
    # finding inverse value of a monotonic function
    searchsortedfirst(array(mysin(x) for x in 0:0.001:3.14), 0.5)


    using Primes
    # first prime greater than 10^6 ( note: primes(10^6, 10^6+100) is much better)
    searchsortedfirst(array(prime(i) for i = 5*10^4:10^5), 10^6)

```

