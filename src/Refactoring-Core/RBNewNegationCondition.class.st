Class {
	#name : 'RBNewNegationCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'condition'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
RBNewNegationCondition class >> on: aRBInstanceVariableHasReferences [ 
	^ self new condition: aRBInstanceVariableHasReferences ; yourself.
]

{ #category : 'checking' }
RBNewNegationCondition >> check [
	^condition check not
]

{ #category : 'accessing' }
RBNewNegationCondition >> condition: aCondition [
	condition := aCondition.
]

{ #category : 'accessing' }
RBNewNegationCondition >> errorString [ 

	^ condition errorString
]

{ #category : 'accessing' }
RBNewNegationCondition >> violators [
	"This is probably wrong because of the negation"
	^ condition violators
]
