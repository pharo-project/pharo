tests
testGivenAMethodNodeWhenSourceAvailableThenSourceReturnedAsResultOfFormatting
	| ast sourceCode |
	sourceCode := (Collection class>>#with:with:) sourceCode.
	ast := RBParser parseMethod: sourceCode.
	self 
		assert: (RBNullFormatter new format: ast)
		equals: sourceCode