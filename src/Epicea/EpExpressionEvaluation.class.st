"
I represent a expression evaluation. For example, a 'Do It' or an 'Inspect It'.
"
Class {
	#name : #EpExpressionEvaluation,
	#superclass : #EpEvent,
	#instVars : [
		'expression'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpExpressionEvaluation class >> expression: anExpression [ 
	^ self basicNew 
		initializeExpression: anExpression;
		yourself
]

{ #category : #comparing }
EpExpressionEvaluation >> = other [

	^ self class = other class 
		and: [ self expression = other expression ]
]

{ #category : #visitor }
EpExpressionEvaluation >> accept: aVisitor [

	^ aVisitor visitExpressionEvaluation: self
]

{ #category : #accessing }
EpExpressionEvaluation >> expression [

	^ expression
]

{ #category : #comparing }
EpExpressionEvaluation >> hash [

	^ self class hash bitXor: self expression hash
]

{ #category : #initialize }
EpExpressionEvaluation >> initializeExpression: anExpression [

	self initialize.
	expression := anExpression.
]

{ #category : #printing }
EpExpressionEvaluation >> printOn: aStream [
	super printOn: aStream.

	aStream 
		nextPut: $(;
		nextPutAll: self expression asString;
		nextPut: $)
]
