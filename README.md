typeinf
=======

Simply-typed lambda calculus type inferer

usage
=====

    ghci TypeInf/TypeInf.hs 
    *TypeInf> let i = "(\\x.x)"
    *TypeInf> let k = "(\\x y.x)"
    *TypeInf> let s = "(\\x y z.x z (y z))"
    *TypeInf> typ i
    λx.x : α->α
    *TypeInf> typ k
    λx y.x : α->(β->α)
    *TypeInf> typ s
    λx y z.(x z) (y z) : (α->(β->γ))->(α->β)->α->γ
    *TypeInf> let skk = s++k++k
    *TypeInf> skk
    "(\\x y z.x z (y z))(\\x y.x)(\\x y.x)"
    *TypeInf> parseLambdaTerm skk
    ((λx y z.(x z) (y z)) (λx y.x)) (λx y.x)
    *TypeInf> typ skk
    ((λx y z.(x z) (y z)) (λx y.x)) (λx y.x) : α->α
    *TypeInf> typ (k++i)
    (λx y.x) (λx.x) : α->(β->β)
    *TypeInf> typ (s++i++i)
    term ((λx y z.(x z) (y z)) (λx.x)) (λx.x) has no type!
    *TypeInf> typ "\\x y.y(\\z.z(y x))"
    λx y.y (λz.z (y x)) : ((α->β)->β)->(((α->β)->β)->γ)->γ
    *TypeInf>

