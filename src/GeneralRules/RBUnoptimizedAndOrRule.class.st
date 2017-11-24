"
Checks for inefficient nesting of logical conditions.
"
Class {
	#name : #RBUnoptimizedAndOrRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
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
	self matchesAny: #(
			'(`@a and: `@b) and: `@c'
			'(`@a or: `@b) or: `@c' )
]

{ #category : #accessing }
RBUnoptimizedAndOrRule >> name [
	^ 'Uses "(a and: [b]) and: [c]" instead of "a and: [b and: [c]]"'
]
