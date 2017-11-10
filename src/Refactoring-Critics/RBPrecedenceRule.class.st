"
Checks for mathematical expressions that might be evaluated different (from left-to-right) than the developer thinks.
"
Class {
	#name : #RBPrecedenceRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBPrecedenceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'PrecedenceRule'
]

{ #category : #accessing }
RBPrecedenceRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBPrecedenceRule >> initialize [
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [node hasParentheses not and: [#(#+ #-) includes: node selector]]} * `@C'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBPrecedenceRule >> name [
	^ 'Inspect instances of "A + B * C" might be "A + (B * C)"'
]

{ #category : #accessing }
RBPrecedenceRule >> rationale [
	^ 'Checks for mathematical expressions that might be evaluated different (from left-to-right) than the developer thinks.'
]
