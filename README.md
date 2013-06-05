typeinf
=======

Simply-typed lambda calculus type inferer

usage
=====

    ghci TypeInf/Inferer.hs 
    *TypeInf.Inferer> let i = "(\\x.x)"
	*TypeInf.Inferer> let k = "(\\x y.x)"
	*TypeInf.Inferer> let s = "(\\x y z.x z(y z))"
	*TypeInf.Inferer> typ i
	λx.x : α->α
	*TypeInf.Inferer> typ k
	λx y.x : α->β->α
	*TypeInf.Inferer> typ s
	λx y z.x z (y z) : (α->β->γ)->(α->β)->α->γ
	*TypeInf.Inferer> let skk = s++k++k
	*TypeInf.Inferer> skk
	"(\\x y z.x z(y z))(\\x y.x)(\\x y.x)"
	*TypeInf.Inferer> typ skk
	(λx y z.x z (y z)) (λx y.x) (λx y.x) : α->α
	*TypeInf.Inferer> typ (k++i)
	(λx y.x) (λx.x) : α->β->β
	*TypeInf.Inferer> typ (s++i++i)
	term (λx y z.x z (y z)) (λx.x) (λx.x) has no type!
	*TypeInf.Inferer> typ "\\x y.y (\\z. z (y x))"
	λx y.y (λz.z (y x)) : ((α->β)->β)->(((α->β)->β)->γ)->γ
	*TypeInf.Inferer> 
