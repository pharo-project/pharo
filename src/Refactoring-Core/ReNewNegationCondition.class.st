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
ReNewNegationCondition >> errorMacro [
	"TODO: This was introduced for compatibility with RBAbstractCondition
	and it should be removed"
	^ self errorString
]

{ #category : 'accessing' }
ReNewNegationCondition >> errorString [ 

	^ condition errorString
]

{ #category : 'logical operations' }
ReNewNegationCondition >> not [ 
	
	^ condition
]

{ #category : 'displaying' }
ReNewNegationCondition >> violationMessageOn: aWriteStream [

	aWriteStream
		nextPutAll: 'NOT ';
		nextPutAll: condition errorString
]

{ #category : 'accessing' }
ReNewNegationCondition >> violators [
	"This is probably wrong because of the negation"
	^ condition nonViolators
]
