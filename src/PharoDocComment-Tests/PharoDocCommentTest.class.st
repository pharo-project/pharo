"
SUnit test for PharoDocComment
"
Class {
	#name : #PharoDocCommentTest,
	#superclass : #TestCase,
	#category : #'PharoDocComment-Tests-Base'
}

{ #category : #helpers }
PharoDocCommentTest class >> stringWithDoubleDoubleQuotes [

	^ 'This is ""exagerated"" at least'
]

{ #category : #helpers }
PharoDocCommentTest class >> stringWithDoubleQuotes [

	^ 'This is "exagerated" at least'
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
	"3 + 4 >>> 7"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression source equals: '3 + 4 >>> 7'.
	self assert: node expression isFaulty not.
	self assert: node expression evaluate equals: 7->7.
	(CommentTestCase for: node) testIt
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionNoAssociation [

	"This one is not syntaxically valid since the last statement is not the triple >"

	"(3 + 4 >>> 7) value"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty.
	self should: [ node expression evaluate ] raise: RuntimeSyntaxError.
	self should: [ (CommentTestCase for: node) testIt ] raise: RuntimeSyntaxError
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionNoAssociation2 [

	"This one is syntaxically valid, but does not return an association at runtime."

	"true ifTrue: [^ 7]. 1 >>> 1"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty not.
	self assert: node expression evaluate equals: 7.
	self should: [ (CommentTestCase for: node) testIt ] raise: TestFailure
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionReferencingSelf [

	"This one shows that self is correclty bound to the class"

	"self >>> PharoDocCommentTest"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty not.
	self assert: node expression evaluate equals: self class -> self class.
	(CommentTestCase for: node) testIt
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionRuntimeError [
	"nil + nil >>> nil"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty not.
	self should: [ node expression evaluate ] raise: Error.
	self should: [ (CommentTestCase for: node) testIt ] raise: Error
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionSyntaxError [
	"3 + 4) >>> 7"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty.
	self should: [ node expression evaluate ] raise: RuntimeSyntaxError.
	self should: [ (CommentTestCase for: node) testIt ] raise: RuntimeSyntaxError
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionWithEscapes [
	"PharoDocCommentTest stringWithDoubleQuotes >>> 'This is ""exagerated"" at least'"

	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression source equals: 'PharoDocCommentTest stringWithDoubleQuotes >>> ''This is ""exagerated"" at least'''.
	self deny: node expression isFaulty.
	self assert: node expression evaluate equals: 'This is "exagerated" at least' -> 'This is "exagerated" at least'.
	(CommentTestCase for: node) testIt
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionWithEscapes2 [
	"This test is here in case one day the Pharo parser is updated to remove the escaped double quotes from the RBCommentNode.
	In that case, the first test will work but this one will fail and we will be able to know we need to update PharoDocComment"

	"PharoDocCommentTest stringWithDoubleDoubleQuotes >>> 'This is """"exagerated"""" at least'"

	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression source equals: 'PharoDocCommentTest stringWithDoubleDoubleQuotes >>> ''This is """"exagerated"""" at least'''.
	self deny: node expression isFaulty.
	self assert: node expression evaluate equals: 'This is ""exagerated"" at least' -> 'This is ""exagerated"" at least'.
	(CommentTestCase for: node) testIt
]

{ #category : #tests }
PharoDocCommentTest >> testExpressionZero [

	"This one feels like a doc comment but there is no real triple > message send in the AST."

	"#( 1 >>> 1 )"
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty.
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
	self assert: espression evaluate equals: 3->3
]

{ #category : #tests }
PharoDocCommentTest >> testMultipleDocCommentsInOneComment [
	"3 + 4 >>> 7.
	1 + 2 >>> 3."
	| node |
	node := thisContext method ast pharoDocCommentNodes first.
	self assert: node expression isFaulty.
	self should: [ node expression evaluate ] raise: RuntimeSyntaxError.
	self should: [ (CommentTestCase for: node) testIt ] raise: RuntimeSyntaxError
]
