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
	subjects := condition subjects
]

{ #category : 'accessing' }
ReNewNegationCondition >> errorString [ 

	^ condition errorString
]

{ #category : 'accessing' }
ReNewNegationCondition >> nonViolators [

	^ subjects difference: self violators
]

{ #category : 'displaying' }
ReNewNegationCondition >> violationMessageOn: aWriteStream [

	aWriteStream
		nextPutAll: 'NOT ';
		nextPutAll: condition errorString
]

{ #category : 'accessing' }
ReNewNegationCondition >> violators [

	^ condition nonViolators
]
