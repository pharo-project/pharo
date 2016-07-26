tests
testLintRuleTypes
	| typer types |
	typer := RBRefactoryTyper new.
	types := typer 
		guessTypesFor: 'name'
		in: RBLintRuleTest.
	self assert: types size = 1.
	self assert: (types includes: (typer model classFor: String))