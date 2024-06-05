"
Checks for inefficient nesting of logical conditions.
"
Class {
	#name : 'ReUnoptimizedAndOrRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReUnoptimizedAndOrRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReUnoptimizedAndOrRule class >> ruleName [
	^ 'Uses "(a and: [b]) and: [c]" instead of "a and: [b and: [c]]"'
]

{ #category : 'accessing' }
ReUnoptimizedAndOrRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'UnoptimizedAndOrRule'
]

{ #category : 'initialization' }
ReUnoptimizedAndOrRule >> initialize [
	super initialize.
	self matchesAny: #(
			'(`@a and: `@b) and: `@c'
			'(`@a or: `@b) or: `@c' )
]
