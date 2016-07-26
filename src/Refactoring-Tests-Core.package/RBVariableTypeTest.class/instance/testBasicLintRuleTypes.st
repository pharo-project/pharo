tests
testBasicLintRuleTypes
	| typer types |
	typer := RBRefactoryTyper new.
	types := typer 
		guessTypesFor: 'classBlock'
		in: RBBasicLintRuleTest.
	"self assert: types size = 1."
	self assert: ([  ] class withAllSuperclasses 
			detect: [ :each | types includes: (typer model classFor: each) ]
			ifNone: [ nil ]) notNil.
	types := typer 
		typesFor: 'methodBlock'
		in: (typer model classFor: RBBasicLintRuleTest).
	"self should: [types size = 2]."
	self assert: ([  ] class withAllSuperclasses 
			detect: [ :each | types includes: (typer model classFor: each) ]
			ifNone: [ nil ]) notNil.
	"self should: [types includes: MessageChannel]."
	typer printString