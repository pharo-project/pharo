"
Checks for inefficient nesting of logical conditions.
"
Class {
	#name : #ReUnoptimizedAndOrRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReUnoptimizedAndOrRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnoptimizedAndOrRule'
]

{ #category : #accessing }
ReUnoptimizedAndOrRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
ReUnoptimizedAndOrRule >> initialize [
	super initialize.
	self matchesAny: #(
			'(`@a and: `@b) and: `@c'
			'(`@a or: `@b) or: `@c' )
]

{ #category : #accessing }
ReUnoptimizedAndOrRule >> name [
	^ 'Uses "(a and: [b]) and: [c]" instead of "a and: [b and: [c]]"'
]
