"
SUnit test for PharoDocComment
"
Class {
	#name : #PharoDocCommentTest,
	#superclass : #TestCase,
	#category : #'PharoDocComment-Tests-Base'
}

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
	"3 + 4 >>> 7"
	| expression |
	expression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: expression source equals: '3 + 4 >>> 7'
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionReferencingSelf [
	"self >>> PharoDocCommentTest"
	| espression |
	espression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: espression evaluate equals: self class.
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionResult [
	"3 + 4 >>> 7"
	| expression |
	expression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: expression evaluate equals: 7
]

{ #category : #tests }
PharoDocCommentTest >> testMultipleDocComments [
	"3 + 4 >>> 7"
	"1 + 2 >>> 3."
	| espression |
	espression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: espression evaluate equals: 7.
	espression := thisContext method ast pharoDocCommentNodes second expression.
	self assert: espression evaluate equals: 3.
]

{ #category : #tests }
PharoDocCommentTest >> testMultipleDocCommentsInOneComment [
	"3 + 4 >>> 7.
	1 + 2 >>> 3."
	| espression |
	espression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: espression evaluate equals: 7.
	espression := thisContext method ast pharoDocCommentNodes second expression.
	self assert: espression evaluate equals: 3.
]

{ #category : #tests }
PharoDocCommentTest >> testNodeResultSource [
	"we use a doc comment here to test"
	"3 + 4 >>> 7"
	
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node result source equals: '3 + 4 >>> 7'
]
