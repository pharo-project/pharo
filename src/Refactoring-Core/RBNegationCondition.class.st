"
I am a refactoring condition representing a boolean NOT of another condition.

I am created by sending the message #not to another condition.

Checking this conditions holds true, if my condition is false. 

"
Class {
	#name : #RBNegationCondition,
	#superclass : #RBAbstractCondition,
	#instVars : [
		'condition'
	],
	#category : #'Refactoring-Core-Conditions'
}

{ #category : #'instance creation' }
RBNegationCondition class >> on: aCondition [
	^self new condition: aCondition
]

{ #category : #checking }
RBNegationCondition >> check [
	^condition check not
]

{ #category : #initialization }
RBNegationCondition >> condition: aCondition [ 
	condition := aCondition.
	self errorMacro: condition errorMacro
]

{ #category : #private }
RBNegationCondition >> errorBlockFor: aBoolean [ 
	^condition errorBlockFor: aBoolean not
]

{ #category : #private }
RBNegationCondition >> errorStringFor: aBoolean [ 
	^condition errorStringFor: aBoolean not
]

{ #category : #printing }
RBNegationCondition >> printOn: aStream [ 
	aStream nextPutAll: 'NOT ';
		print: condition
]
