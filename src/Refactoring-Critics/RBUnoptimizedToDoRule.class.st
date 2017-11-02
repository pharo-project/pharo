"
Checks for inefficient uses of to:do: that create an unnecessary Interval instance.

( 1 to: 10 ) do: aBlock

can be more efficiently expressed as 

1 to: 10 do: aBlock
"
Class {
	#name : #RBUnoptimizedToDoRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBUnoptimizedToDoRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnoptimizedToDoRule'
]

{ #category : #accessing }
RBUnoptimizedToDoRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBUnoptimizedToDoRule >> initialize [
	super initialize.
	self matcher 
		matches: '(`@a to: `@b) do: `@c'
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBUnoptimizedToDoRule >> name [
	^ 'Uses (to:)do: instead of to:do:'
]

{ #category : #accessing }
RBUnoptimizedToDoRule >> rationale [
	^ 'Checks for inefficient uses of to:do: that create an unnecessary Interval instance.'
]
