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
    *TypeInf> 

