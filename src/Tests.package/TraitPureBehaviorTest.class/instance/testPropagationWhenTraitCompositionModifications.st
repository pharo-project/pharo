testing-applying trait composition
testPropagationWhenTraitCompositionModifications
	"Test that the propagation mechanism works when
	setting new traitCompositions."

	self assert: self c2 methodDict size = 9.	"2 + (3+(3+2))-1"

	"removing methods"
	Trait 
		named: #T5
		uses: self t1 + self t2 - { #m21. #m22 }
		category: self class category.
	self assert: self c2 methodDict size = 7.

	"adding methods"
	Trait 
		named: #T2
		uses: self t3
		category: self class category.
	self assert: self c2 methodDict size = 10.
	self assert: (self c2 selectors asSet includesAll: #(#m31 #m32 #m33 ))