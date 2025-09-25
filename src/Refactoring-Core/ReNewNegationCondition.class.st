Class {
	#name : 'ReNewNegationCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'condition'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReNewNegationCondition class >> on: aRBInstanceVariableHasReferences [ 
	^ self new condition: aRBInstanceVariableHasReferences ; yourself.
]

{ #category : 'checking' }
ReNewNegationCondition >> check [
	^condition check not
]

{ #category : 'accessing' }
ReNewNegationCondition >> condition: aCondition [
	condition := aCondition.
]

{ #category : 'accessing' }
ReNewNegationCondition >> errorString [ 

	^ String streamContents: [ :aStream |
		self violationMessageOn: aStream ]
]

{ #category : 'accessing' }
ReNewNegationCondition >> nonViolators [
	^ condition violators
]

{ #category : 'displaying' }
ReNewNegationCondition >> violationMessageOn: aWriteStream [
	condition wrapper: self.
	aWriteStream
		nextPutAll: 'NOT ';
		nextPutAll: condition errorString.
	condition wrapper: nil
]

{ #category : 'accessing' }
ReNewNegationCondition >> violators [
	"This is probably wrong because of the negation"
	^ condition nonViolators
]
