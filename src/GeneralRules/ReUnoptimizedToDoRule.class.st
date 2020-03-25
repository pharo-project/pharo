"
Checks for inefficient uses of to:do: that create an unnecessary Interval instance.

( 1 to: 10 ) do: aBlock

can be more efficiently expressed as 

1 to: 10 do: aBlock
"
Class {
	#name : #ReUnoptimizedToDoRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReUnoptimizedToDoRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnoptimizedToDoRule'
]

{ #category : #accessing }
ReUnoptimizedToDoRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
ReUnoptimizedToDoRule >> initialize [
	super initialize.
	self matches: '(`@a to: `@b) do: `@c'
		
]

{ #category : #accessing }
ReUnoptimizedToDoRule >> name [
	^ 'Uses (to:)do: instead of to:do:'
]
