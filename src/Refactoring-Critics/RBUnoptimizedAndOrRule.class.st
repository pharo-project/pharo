"
Checks for inefficient nesting of logical conditions.
"
Class {
	#name : #RBUnoptimizedAndOrRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBUnoptimizedAndOrRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnoptimizedAndOrRule'
]

{ #category : #accessing }
RBUnoptimizedAndOrRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBUnoptimizedAndOrRule >> initialize [
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'(`@a and: `@b) and: `@c'
			'(`@a or: `@b) or: `@c' )
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBUnoptimizedAndOrRule >> name [
	^ 'Uses "(a and: [b]) and: [c]" instead of "a and: [b and: [c]]"'
]
