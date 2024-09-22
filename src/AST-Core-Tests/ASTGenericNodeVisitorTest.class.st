Class {
	#name : 'ASTGenericNodeVisitorTest',
	#superclass : 'ASTParseTreeTest',
	#category : 'AST-Core-Tests-Visitors',
	#package : 'AST-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
ASTGenericNodeVisitorTest >> testVisitAnySatisfy [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := ASTGenericNodeVisitor visit: tree anySatisfy: [ :n | n isMessage ].
	self assert: result.
	result := ASTGenericNodeVisitor visit: tree anySatisfy: [ :n | n isReturn ].
	self deny: result
]

{ #category : 'tests' }
ASTGenericNodeVisitorTest >> testVisitDetect [
	| node tree |
	tree := self parseExpression: 'Object new'.
	node := ASTGenericNodeVisitor  visit: tree detect: [ :n | n isMessage ].
	self  assert: node isMessage
]

{ #category : 'tests' }
ASTGenericNodeVisitorTest >> testVisitDetectIfNone [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := ASTGenericNodeVisitor visit: tree detect: [ :n | n isReturn ] ifNone: [ #none ].
	self assert: result equals: #none
]

{ #category : 'tests' }
ASTGenericNodeVisitorTest >> testVisitNoneSatisfy [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := ASTGenericNodeVisitor visit: tree noneSatisfy: [ :n | n isMessage ].
	self deny: result.
	result := ASTGenericNodeVisitor visit: tree noneSatisfy: [ :n | n isReturn ].
	self assert: result
]

{ #category : 'tests' }
ASTGenericNodeVisitorTest >> testVisitSelect [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := ASTGenericNodeVisitor visit: tree select: [ :n | n isMessage ].
	self assert: result size equals: 1.
	self assert: result first selector equals: #new
]
