Class {
	#name : #RBVariableTypeTest,
	#superclass : #RBRefactoringBrowserTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBVariableTypeTest >> testBasicLintRuleTypes [
	| typer types |
	typer := RBRefactoryTyper new.
	types := typer 
		guessTypesFor: 'classBlock'
		in: RBBasicLintRuleTestData.
	"self assert: types size = 1."
	self assert: ([  ] class withAllSuperclasses 
			detect: [ :each | types includes: (typer model classFor: each) ]
			ifNone: [ nil ]) notNil.
	types := typer 
		typesFor: 'methodBlock'
		in: (typer model classFor: RBBasicLintRuleTestData).
	"self should: [types size = 2]."
	self assert: ([  ] class withAllSuperclasses 
			detect: [ :each | types includes: (typer model classFor: each) ]
			ifNone: [ nil ]) notNil.
	"self should: [types includes: MessageChannel]."
	typer printString
]

{ #category : #tests }
RBVariableTypeTest >> testCompositeLintRuleTypes [
	| typer types |
	typer := RBRefactoryTyper new runOn: RBCompositeLintRuleTestData.
	types := typer guessTypesFor: 'rules'.
	self assert: (types includes: (typer model classFor: Collection)).
	types := typer typesFor: '-rules-'.
	self assert: (types includes: (typer model classFor: RBLintRuleTestData)).
	self assertEmpty: (typer guessTypesFor: 'asdf').
	typer printString
]

{ #category : #tests }
RBVariableTypeTest >> testLintRuleTypes [
	| typer types |
	typer := RBRefactoryTyper new.
	types := typer 
		guessTypesFor: 'name'
		in: RBLintRuleTestData.
	self assert: types size equals: 1.
	self assert: (types includes: (typer model classFor: String))
]

{ #category : #tests }
RBVariableTypeTest >> testParseTreeTypes [
	| types model |
	model := RBClassModelFactory rbNamespace new.
	types := RBRefactoryTyper 
				typesFor: 'foo'
				in: (self 
						parseExpression: 'foo printString; testBasicLintRuleTypes; testParseTreeTypes')
				model: model.
	self assert: types size equals: 1.
	self assert: (types includes: (model classFor: self class))
]
