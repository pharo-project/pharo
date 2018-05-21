"
I am represent an expression within a String (typically the contents of a RBCommentNode resp. the comment text of a method. I am used as expression or the expected expression value by a PharoDocCommentNode.
"
Class {
	#name : #PharoDocCommentExpression,
	#superclass : #Object,
	#instVars : [
		'expressionInterval',
		'source'
	],
	#category : #'PharoDocComment-Base'
}

{ #category : #operation }
PharoDocCommentExpression >> evaluate [

	^ Smalltalk compiler evaluate: self expression
]

{ #category : #accessing }
PharoDocCommentExpression >> expression [
	^ self source copyFrom: expressionInterval first to: expressionInterval last
]

{ #category : #accessing }
PharoDocCommentExpression >> expressionCode [
	^ (RBParser parseFaultyExpression: self expression) doSemanticAnalysis; yourself
	
]

{ #category : #accessing }
PharoDocCommentExpression >> expressionInterval [
	^ expressionInterval
]

{ #category : #accessing }
PharoDocCommentExpression >> expressionInterval: anObject [
	expressionInterval := anObject
]

{ #category : #printing }
PharoDocCommentExpression >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: '('.
	aStream nextPutAll: self expression. 
	aStream nextPutAll: ')'.
]

{ #category : #accessing }
PharoDocCommentExpression >> source [
	^ source
]

{ #category : #accessing }
PharoDocCommentExpression >> source: anObject [
	source := anObject
]
