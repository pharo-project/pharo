"
SUnit tests for RBCommentNodeVisitor
"
Class {
	#name : 'ASTCommentNodeVisitorTest',
	#superclass : 'ASTParseTreeTest',
	#category : 'AST-Core-Tests-Visitors',
	#package : 'AST-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
ASTCommentNodeVisitorTest >> testVisitDetect [
	| node tree |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	node := ASTCommentNodeVisitor visit: tree detect: [ :n | n intersectsInterval: (20 to: 20) ].

	self
		assert: node contents equals: 'comment 2'
]

{ #category : 'tests' }
ASTCommentNodeVisitorTest >> testVisitDo [
	| node tree count |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	count := 0.
	node := ASTCommentNodeVisitor visit: tree do: [ :n | count := count + 1 ].

	self
		assert: count equals: 3
]

{ #category : 'tests' }
ASTCommentNodeVisitorTest >> testVisitSelect [
	| node tree |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	node := ASTCommentNodeVisitor visit: tree select: [ :n | n contents endsWith: '2'].

	self
		assert: node first contents equals: 'comment 2'
]
