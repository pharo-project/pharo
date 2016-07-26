tests
testCompositeLintRuleTypes
	| typer types |
	typer := RBRefactoryTyper new runOn: RBCompositeLintRuleTest.
	types := typer guessTypesFor: 'rules'.
	self assert: (types includes: (typer model classFor: Collection)).
	types := typer typesFor: '-rules-'.
	self assert: (types includes: (typer model classFor: RBLintRuleTest)).
	self assert: (typer guessTypesFor: 'asdf') isEmpty.
	typer printString