"
SUnit test for PharoDocComment
"
Class {
	#name : #PharoDocCommentTest,
	#superclass : #TestCase,
	#instVars : [
		'expression'
	],
	#category : #'PharoDocComment-Tests-Base'
}

{ #category : #running }
PharoDocCommentTest >> setUp [
	| nodes |
	super setUp.
	nodes := (ClassWithPharoDocComment >> #methodWithCorrectExpression) ast pharoDocCommentNodes.
	expression := nodes first expression
]

{ #category : #tests }
PharoDocCommentTest >> testExpression [

	self assert: expression source equals: '3 + 4 >>> 7'
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionResult [

	self assert: expression evaluate equals: 7
]
