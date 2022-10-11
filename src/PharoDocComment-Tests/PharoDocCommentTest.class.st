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
PharoDocCommentTest >> testExpressionNoAssociation [
	"(3 + 4 >>> 7) value"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression evaluate equals: 7.
	self should: [ (CommentTestCase for: node) testIt ] raise: TestFailure
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionReferencingSelf [
	"self >>> PharoDocCommentTest"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression evaluate equals: self class -> self class.
	(CommentTestCase for: node) testIt.
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionResult [
	"3 + 4 >>> 7"
	| expression |
	expression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: expression evaluate equals: 7->7
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionRuntimeError [
	"nil + nil >>> nil"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self should: [ node expression evaluate ] raise: Error.
	self should: [ (CommentTestCase for: node) testIt ] raise: Error
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionSyntaxError [
	"3 + 4) >>> 7"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self should: [ node expression evaluate ] raise: RuntimeSyntaxError.
	self should: [ (CommentTestCase for: node) testIt ] raise: RuntimeSyntaxError
]

{ #category : #tests }
PharoDocCommentTest >> testMultipleDocComments [
	"3 + 4 >>> 7"
	"1 + 2 >>> 3."
	| espression |
	espression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: espression evaluate equals: 7->7.
	espression := thisContext method ast pharoDocCommentNodes second expression.
	self assert: espression evaluate equals: 3->3.
]

{ #category : #tests }
PharoDocCommentTest >> testMultipleDocCommentsInOneComment [
	"3 + 4 >>> 7.
	1 + 2 >>> 3."
	| espression |
	espression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: espression evaluate equals: 3->3.
]

{ #category : #tests }
PharoDocCommentTest >> testNodeResultSource [
	"we use a doc comment here to test"
	"3 + 4 >>> 7"
	
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression source equals: '3 + 4 >>> 7'
]
