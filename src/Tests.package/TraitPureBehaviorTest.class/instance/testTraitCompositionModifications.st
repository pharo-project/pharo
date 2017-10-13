testing-applying trait composition
testTraitCompositionModifications
	self assert: self t6 methodDict size = 6.
	self assert: (self t6 sourceCodeAt: #m22Alias) asString = 'm22Alias ^22'.
	self t6 setTraitComposition: self t2 asTraitComposition.
	self assert: self t6 methodDict size = 2.
	self deny: (self t6 methodDict includesKey: #m22Alias).
	self t6 
		setTraitCompositionFrom: self t1 @ { (#m13Alias -> #m13) } - { #m11. #m12 } 
				+ self t2.
	self assert: self t6 methodDict size = 4.
	self 
		assert: (self t6 selectors asSet includesAll: #(#m13 #m13Alias #m21 #m22 )).
	self 
		assert: (self t6 sourceCodeAt: #m13Alias) asString = 'm13Alias ^self m12'