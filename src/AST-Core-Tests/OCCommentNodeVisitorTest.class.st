"
SUnit tests for ASTCommentNodeVisitor
"
Class {
	#name : 'OCCommentNodeVisitorTest',
	#superclass : 'OCParseTreeTest',
	#category : 'AST-Core-Tests-Visitors',
	#package : 'AST-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
OCCommentNodeVisitorTest >> testVisitDetect [
	| node tree |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	node := OCCommentNodeVisitor visit: tree detect: [ :n | n intersectsInterval: (20 to: 20) ].

	self
		assert: node contents equals: 'comment 2'
]

{ #category : 'tests' }
OCCommentNodeVisitorTest >> testVisitDo [
	| node tree count |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	count := 0.
	node := OCCommentNodeVisitor visit: tree do: [ :n | count := count + 1 ].

	self
		assert: count equals: 3
]

{ #category : 'tests' }
OCCommentNodeVisitorTest >> testVisitSelect [
	| node tree |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	node := OCCommentNodeVisitor visit: tree select: [ :n | n contents endsWith: '2'].

	self
		assert: node first contents equals: 'comment 2'
]
