typeinf
=======

Simply-typed lambda calculus type inferer

usage
=====

    ghci typeinf.hs
    *TypeInf> let i = (Abs "x" (Var "x"))
    *TypeInf> i
    λx.x
    *TypeInf> let k = (MultiAbs ["x","y"] (Var "x"))
    *TypeInf> k
    λx y.x
    *TypeInf> let xz = (App (Var "x") (Var "z"))
    *TypeInf> let yz = (App (Var "y") (Var "z"))
    *TypeInf> let s = MultiAbs ["x","y","z"] (App xz yz)
    *TypeInf> s
    λx y z.(x z) (y z)
    *TypeInf> printType i
    λx.x : α->α
    *TypeInf> printType k
    λx y.x : α->(β->α)
    *TypeInf> printType s
    λx y z.(x z) (y z) : (α->(β->γ))->(α->β)->α->γ
    *TypeInf> let skk = (App (App s k) k)
    *TypeInf> skk
    ((λx y z.(x z) (y z)) (λx y.x)) (λx y.x)
    *TypeInf> printType skk
    ((λx y z.(x z) (y z)) (λx y.x)) (λx y.x) : α->α
    *TypeInf> let ki = App k i
    *TypeInf> ki
    (λx y.x) (λx.x)
    *TypeInf> printType ki
    (λx y.x) (λx.x) : α->(β->β)
    *TypeInf> 


todo
====
* parser
* beta reduction
* REPL environment

