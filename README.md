# GeneratorArrays

Effectively provide an `AbstractArray` view of a generator.

That means it will be possible to pass a generator to methods expecting
arrays. For example

```
    using GeneratorArrays

    p25 = searchsortedfirst(array(f(x) for x in 1:n)), 2.5)

```

