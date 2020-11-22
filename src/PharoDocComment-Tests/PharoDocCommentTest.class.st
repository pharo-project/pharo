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
PharoDocCommentTest >> testAssociation [

	|association|
	association := 3 + 4 >>> 7.
	self 
		assert: association key equals: 7;
		assert: association value equals: 7
]

{ #category : #tests }
PharoDocCommentTest >> testExpression [

	self assert: expression source equals: '3 + 4 >>> 7'
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionResult [

	self assert: expression evaluate equals: 7
]
