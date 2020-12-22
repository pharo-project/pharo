Class {
	#name : #RBGenericNodeVisitorTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Visitors'
}

{ #category : #tests }
RBGenericNodeVisitorTest >> testVisitAnySatisfy [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := RBGenericNodeVisitor visit: tree anySatisfy: [ :n | n isMessage ].
	self assert: result.
	result := RBGenericNodeVisitor visit: tree anySatisfy: [ :n | n isReturn ].
	self deny: result
]

{ #category : #tests }
RBGenericNodeVisitorTest >> testVisitDetect [
	| node tree |
	tree := self parseExpression: 'Object new'.
	node := RBGenericNodeVisitor  visit: tree detect: [ :n | n isMessage ].
	self  assert: node isMessage
]

{ #category : #tests }
RBGenericNodeVisitorTest >> testVisitDetectIfNone [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := RBGenericNodeVisitor visit: tree detect: [ :n | n isReturn ] ifNone: [ #none ].
	self assert: result equals: #none
]

{ #category : #tests }
RBGenericNodeVisitorTest >> testVisitNoneSatisfy [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := RBGenericNodeVisitor visit: tree noneSatisfy: [ :n | n isMessage ].
	self deny: result.
	result := RBGenericNodeVisitor visit: tree noneSatisfy: [ :n | n isReturn ].
	self assert: result
]

{ #category : #tests }
RBGenericNodeVisitorTest >> testVisitSelect [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := RBGenericNodeVisitor visit: tree select: [ :n | n isMessage ].
	self assert: result size equals: 1.
	self assert: result first selector equals: #new
]
