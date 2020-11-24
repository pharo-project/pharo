"
SUnit test for PharoDocCommentNode
"
Class {
	#name : #PharoDocCommentNodeTest,
	#superclass : #TestCase,
	#category : #'PharoDocComment-Tests-Base'
}

{ #category : #tests }
PharoDocCommentNodeTest >> testNodeResultSource [

	| node |
	node := (ClassWithPharoDocComment >> #methodWithCorrectExpression) ast pharoDocCommentNodes first.
	self assert: node result source equals: '3 + 4 >>> 7'
]
