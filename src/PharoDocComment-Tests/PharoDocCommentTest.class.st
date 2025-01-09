"
SUnit test for PharoDocComment
"
Class {
	#name : 'PharoDocCommentTest',
	#superclass : 'TestCase',
	#category : 'PharoDocComment-Tests-Base',
	#package : 'PharoDocComment-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
PharoDocCommentTest >> testAssociation [

	|association|
	association := 3 + 4 >>> 7.
	self
		assert: association key equals: 7;
		assert: association value equals: 7
]

{ #category : 'tests' }
PharoDocCommentTest >> testExpression [
	"3 + 4 >>> 7"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression source equals: '3 + 4 >>> 7'.
	self assert: node expression isFaulty not.
	self assert: node expression evaluate equals: 7->7.
	(CommentTestCase for: node) testIt
]

{ #category : 'tests' }
PharoDocCommentTest >> testExpressionNoAssociation [

	"This one is not syntaxically valid since the last statement is not the triple >"

	"(3 + 4 >>> 7) value"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self deny: node expression isWellFormed.
	self should: [ node expression evaluate ] raise: Error.
	self should: [ (CommentTestCase for: node) testIt ] raise: Error
]

{ #category : 'tests' }
PharoDocCommentTest >> testExpressionNoAssociation2 [

	"This one is syntaxically valid, but does not return an association at runtime."

	"true ifTrue: [^ 7]. 1 >>> 1"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty not.
	self assert: node expression evaluate equals: 7.
	self should: [ (CommentTestCase for: node) testIt ] raise: TestFailure
]

{ #category : 'tests' }
PharoDocCommentTest >> testExpressionReferencingSelf [

	"This one shows that self is correclty bound to the class"

	"self >>> PharoDocCommentTest"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty not.
	self assert: node expression evaluate equals: self class -> self class.
	(CommentTestCase for: node) testIt
]

{ #category : 'tests' }
PharoDocCommentTest >> testExpressionRuntimeError [
	"nil + nil >>> nil"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty not.
	self should: [ node expression evaluate ] raise: Error.
	self should: [ (CommentTestCase for: node) testIt ] raise: Error
]

{ #category : 'tests' }
PharoDocCommentTest >> testExpressionSyntaxError [
	"3 + 4) >>> 7"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	"We get a syntax error"
	self assert: node expression isFaulty.
	"but the structure of the >>> looks ok"
	self assert: node expression isWellFormed.
	self should: [ node expression evaluate ] raise: OCCodeError.
	self should: [ (CommentTestCase for: node) testIt ] raise: OCCodeError
]

{ #category : 'tests' }
PharoDocCommentTest >> testExpressionZero [

	"This one feels like a doc comment but there is no real triple > message send in the AST."

	"#( 1 >>> 1 )"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty.
	self should: [ node expression evaluate ] raise: Error.
	self should: [ (CommentTestCase for: node) testIt ] raise: Error
]

{ #category : 'tests' }
PharoDocCommentTest >> testMultipleDocComments [
	"3 + 4 >>> 7"
	"1 + 2 >>> 3."
	| espression |
	espression := thisContext method ast pharoDocCommentNodes first expression.
	self assert: espression evaluate equals: 7->7.
	espression := thisContext method ast pharoDocCommentNodes second expression.
	self assert: espression evaluate equals: 3->3
]

{ #category : 'tests' }
PharoDocCommentTest >> testMultipleDocCommentsInOneComment [
	"3 + 4 >>> 7.
	1 + 2 >>> 3."
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty.
	self should: [ node expression evaluate ] raise: Error.
	self should: [ (CommentTestCase for: node) testIt ] raise: Error
]

{ #category : 'tests' }
PharoDocCommentTest >> testNestedComments [
	"3 + 4 ""the sum"" >>> ""is seven"" 7 "
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression source equals: '3 + 4 "the sum" >>> "is seven" 7 '.
	self assert: node expression isFaulty not.
	self assert: node expression evaluate equals: 7->7.
	(CommentTestCase for: node) testIt
]

{ #category : 'tests' }
PharoDocCommentTest >> testNestedInsanity [
	"""Seriously, you should not do what follows""
		""3 + 4 """"a nested executable comment with a nested nested comment
		because it's how we roll baby"""" >>> 7""
	""that is not the test, it's still the executable comment""
	| node |
	node := thisContext method ast allComments second pharoDocCommentNodes first.
	node expression evaluate >>> (7->7)
	""This is the end of the executable comment"""

	"Now it's the real test... or is it?"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression source equals: '"Seriously, you should not do what follows"
		"3 + 4 ""a nested executable comment with a nested nested comment
		because it''s how we roll baby"" >>> 7"
	"that is not the test, it''s still the executable comment"
	| node |
	node := thisContext method ast allComments second pharoDocCommentNodes first.
	node expression evaluate >>> (7->7)
	"This is the end of the executable comment"'.
	self assert: node expression isFaulty not.
	self assert: node expression evaluate equals: ((7->7)->(7->7)).
	(CommentTestCase for: node) testIt
]
