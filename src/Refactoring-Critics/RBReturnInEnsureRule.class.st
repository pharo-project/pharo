"
Checks for return statements within ensure: blocks that can have unintended side-effects.
"
Class {
	#name : #RBReturnInEnsureRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBReturnInEnsureRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ReturnInEnsureRule'
]

{ #category : #accessing }
RBReturnInEnsureRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBReturnInEnsureRule >> initialize [
	| returnMatcher |
	super initialize.
	returnMatcher := RBParseTreeSearcher new.
	returnMatcher 
		matches: '^ `@object'
		do: [ :node :answer | true ].
	self matcher 
		matchesAnyOf: #(
			'``@rcv ensure: [| `@temps | ``@.Stmts]'
			'``@rcv ifCurtailed: [| `@temps | ``@.Stmts]')
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(returnMatcher executeTree: node arguments first initialAnswer: false) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #accessing }
RBReturnInEnsureRule >> name [
	^ 'Contains a return in an ensure: block'
]
