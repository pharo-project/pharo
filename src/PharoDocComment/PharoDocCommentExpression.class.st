"
I am represent an expression within a String (typically the contents of a RBCommentNode resp. the comment text of a method. I am used as expression or the expected expression value by a PharoDocCommentNode.
"
Class {
	#name : #PharoDocCommentExpression,
	#superclass : #Object,
	#instVars : [
		'source',
		'node',
		'cachedExpressionNode'
	],
	#category : #'PharoDocComment-Base'
}

{ #category : #operation }
PharoDocCommentExpression >> evaluate [

	"Note: it seems there is no easy way here to reuse in Opal the parsed AST and just compile&evaluate it."

	^ self methodClass compiler
		  source: self source;
		  noPattern: true;
		  receiver: self methodClass;
		  options: #( + optionParseErrors );
		  evaluate
]

{ #category : #accessing }
PharoDocCommentExpression >> expressionNode [

	cachedExpressionNode ifNil: [
		cachedExpressionNode := self expressionNodeReal
	].
	^ cachedExpressionNode.
]

{ #category : #accessing }
PharoDocCommentExpression >> expressionNodeReal [

	"Force the parsing of the expression node"

	^ self methodClass compiler
		  source: self source;
		  noPattern: true;
		  receiver: self methodClass;
		  options: #( + optionParseErrors );
		  parse.
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
	aStream nextPutAll: self source. 
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
