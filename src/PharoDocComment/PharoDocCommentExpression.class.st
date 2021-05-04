"
I am represent an expression within a String (typically the contents of a RBCommentNode resp. the comment text of a method. I am used as expression or the expected expression value by a PharoDocCommentNode.
"
Class {
	#name : #PharoDocCommentExpression,
	#superclass : #Object,
	#instVars : [
		'expressionInterval',
		'source',
		'node'
	],
	#category : #'PharoDocComment-Base'
}

{ #category : #operation }
PharoDocCommentExpression >> evaluate [

	^ self methodClass compiler
		  source: self expression;
		  noPattern: true;
		  receiver: self methodClass;
		  options: #( + optionParseErrors );
		  evaluate
]

{ #category : #accessing }
PharoDocCommentExpression >> expression [
	^ self source copyFrom: expressionInterval first to: expressionInterval last
]

{ #category : #accessing }
PharoDocCommentExpression >> expressionCode [

	^ self methodClass compiler
		  source: self expression;
		  noPattern: true;
		  options: #( + optionParseErrors );
		  parse
]

{ #category : #accessing }
PharoDocCommentExpression >> expressionInterval [
	^ expressionInterval
]

{ #category : #accessing }
PharoDocCommentExpression >> expressionInterval: anObject [
	expressionInterval := anObject
]

{ #category : #operation }
PharoDocCommentExpression >> methodClass [
	^ node sourceNode methodNode methodClass
]

{ #category : #accessing }
PharoDocCommentExpression >> node [
	^ node
]

{ #category : #accessing }
PharoDocCommentExpression >> node: anObject [
	node := anObject
]

{ #category : #printing }
PharoDocCommentExpression >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: '('.
	aStream nextPutAll: self expression. 
	aStream nextPutAll: ')'
]

{ #category : #accessing }
PharoDocCommentExpression >> source [
	^ source
]

{ #category : #accessing }
PharoDocCommentExpression >> source: anObject [
	source := anObject
]
