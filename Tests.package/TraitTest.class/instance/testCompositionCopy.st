testing
testCompositionCopy
	| t6compositionCopyFirst c2compositionCopy |
	self assert: (self t1 + self t2) allTraits 
				= (self t1 + self t2) copyTraitExpression allTraits.
	self assert: (self t1 classTrait + self t2 classTrait) allTraits 
				= (self t1 classTrait + self t2 classTrait) copyTraitExpression allTraits.
	self assert: self t6 traitComposition allTraits 
				= self t6 traitComposition copyTraitExpression allTraits.
	self 
		assert: (self t6 asTraitComposition copyTraitExpression allTraits includesAll: { (self t1). (self t2). (self t6) }).
	"make no undue sharing happens of exclusions and aliases after an expression copy"
	t6compositionCopyFirst := self t6 traitComposition copyTraitExpression.
	t6compositionCopyFirst transformations at: 1 put: #m22Alias -> #m33.
	self 
		assert: self t6 traitComposition transformations second aliases first value 
				= #m22.
	c2compositionCopy := self c2 traitComposition copyTraitExpression.
	c2compositionCopy transformations first exclusions at: 1 put: #m4.
	self c2 traitComposition transformations first exclusions = #(#m11 )