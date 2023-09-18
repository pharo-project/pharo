"
SUnit tests for RBCommentNodeVisitor
"
Class {
	#name : #RBCommentNodeVisitorTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Visitors'
}

{ #category : #tests }
RBCommentNodeVisitorTest >> testVisitDetect [
	| node tree |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	node := RBCommentNodeVisitor visit: tree detect: [ :n | n intersectsInterval: (20 to: 20) ].

	self
		assert: node contents equals: 'comment 2'
]

{ #category : #tests }
RBCommentNodeVisitorTest >> testVisitDo [
	| node tree count |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	count := 0.
	node := RBCommentNodeVisitor visit: tree do: [ :n | count := count + 1 ].

	self
		assert: count equals: 3
]

{ #category : #tests }
RBCommentNodeVisitorTest >> testVisitSelect [
	| node tree |
	tree := self parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.

	node := RBCommentNodeVisitor visit: tree select: [ :n | n contents endsWith: '2'].

	self
		assert: node first contents equals: 'comment 2'
]
