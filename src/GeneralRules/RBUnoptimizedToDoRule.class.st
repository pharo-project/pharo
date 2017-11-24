"
Checks for inefficient uses of to:do: that create an unnecessary Interval instance.

( 1 to: 10 ) do: aBlock

can be more efficiently expressed as 

1 to: 10 do: aBlock
"
Class {
	#name : #RBUnoptimizedToDoRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
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
	self matches: '(`@a to: `@b) do: `@c'
		
]

{ #category : #accessing }
RBUnoptimizedToDoRule >> name [
	^ 'Uses (to:)do: instead of to:do:'
]
