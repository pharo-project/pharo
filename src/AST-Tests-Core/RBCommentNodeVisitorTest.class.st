Class {
	#name : #RBCommentNodeVisitorTest,
	#superclass : #TestCase,
	#category : #'AST-Tests-Core'
}

{ #category : #tests }
RBCommentNodeVisitorTest >> testVisitDetect [
	| node tree |
	tree := RBParser parseExpression: '
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
	tree := RBParser parseExpression: '
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
	tree := RBParser parseExpression: '
	"comment 1"
	"comment 2"
	"comment 3"
	^true'.
	
	node := RBCommentNodeVisitor visit: tree select: [ :n | n contents endsWith: '2'].
	
	self 
		assert: node first contents equals: 'comment 2'
]
