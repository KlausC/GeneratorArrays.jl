using GeneratorArrays
using Test

@testset "GeneratorArrays" begin

    @testset "iterators" begin
        let i0 = 1:0.5:5, i1 = [1;2;3]; i2 = [Float16(1) 2; 3 4; 5 6]
        @testset "arrays $it" for (it,T,N) in ((i0,Float64,1),
                                               (i1,Int,1),
                                               (i2,Float16,2))
            @test array(it) === it
            gen = (x for x in it)
            @test length(axes(array(gen))) == N
            agen = array(gen)
            @test axes(agen) == axes(it) 
            @test size(agen) == size(it) 
            @test length(agen) == length(it)
            @test iterate(agen) == iterate(it)
            @test agen == collect(it)
            @test agen[3] == it[3]
        end

        @test_throws DomainError array("abc") 
        end
    end

    @testset "generators" begin
        a = [1.0; 2; 3]
        let g1 = (Float16(sin(x)) for x in 1:5), g3 = (x*y for x in a, y in 1:5, z = 1:2)
        @testset "generator dim $N" for (it,T,N,s) in ((g1,Float16,1,(5,)),
                                                    (g3,Float64,3,(3,5,2)))
            @test array(it).gen === it
            agen = array(it)
            @test length(axes(agen)) == N
            @test size(agen) == s
            @test length(agen) == prod(s)
            @test collect(agen) == collect(it)
            @test agen == collect(it)
            @test agen[1] == iterate(it)[1]
        end
        agen = array(g1)
        @test agen[3] == Float16(sin(3.0))
        agen = array(g3)
        @test agen[1] == 1
        @test agen[1,2] == 2
        @test agen[1,2,2] == 2
        @test_throws BoundsError agen[1,2,3]
        @test agen[1,2,1,1] == 2
        @test_throws BoundsError agen[]
        @test_throws BoundsError agen[1,1,1,17]

        @test_throws DomainError array((x*y for x in a for y in a))
        end
    end

    @testset "recursive" begin
        let g0 = (x^2 for x in -2:2), g1 = (x*y for x in 1:2, y in 0:10)
            
            g2 = ((s, t) for s in g0, t in g1)
            a2 = array(g2)
            @test size(a2) == (5,2,11)
            @test eltype(a2) == Tuple{Int,Int}
            @test a2 == collect(g2)
        end
    end 
end

