tests
testGivenAMethodNodeWhenSourceNotAvailableThenSelectorWithWarningMessageReturnedAsResultOfFormatting
	| ast sourceCode |
	sourceCode := (Collection class>>#with:with:) sourceCode.
	ast := RBParser parseMethod: sourceCode.
	ast source: nil.
	self 
		assert: (RBNullFormatter new format: ast)
		equals: 'with:with:
	''no source and no formatter available!'''