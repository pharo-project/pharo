"
SUnit test for PharoDocCommentNode
"
Class {
	#name : #PharoDocCommentNodeTest,
	#superclass : #TestCase,
	#instVars : [
		'node'
	],
	#category : #'PharoDocComment-Tests-Base'
}

{ #category : #running }
PharoDocCommentNodeTest >> setUp [
	| nodes |
	super setUp.
	nodes := (ClassWithPharoDocComment >> #methodWithCorrectExpression) ast pharoDocCommentNodes.
	node := nodes first 
]

{ #category : #tests }
PharoDocCommentNodeTest >> testNodeResultSource [

	self assert: node result source equals: '3 + 4 >>> 7'
]
