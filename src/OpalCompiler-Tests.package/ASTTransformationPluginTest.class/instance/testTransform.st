tests
testTransform
		| ast |
		ast := (OCOpalExamples>>#exampleReturn42) ast copy.
		self assert: ast body statements first value value =  42.
		ast := ASTPluginMeaningOfLife transform: ast.
		self assert: ast body statements first value value =  'meaning of life'.