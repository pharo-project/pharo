Class {
	#name : 'OCGenericNodeVisitorTest',
	#superclass : 'OCParseTreeTest',
	#category : 'AST-Core-Tests-Visitors',
	#package : 'AST-Core-Tests',
	#tag : 'Visitors'
}

{ #category : 'tests' }
OCGenericNodeVisitorTest >> testVisitAnySatisfy [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := OCGenericNodeVisitor visit: tree anySatisfy: [ :n | n isMessage ].
	self assert: result.
	result := OCGenericNodeVisitor visit: tree anySatisfy: [ :n | n isReturn ].
	self deny: result
]

{ #category : 'tests' }
OCGenericNodeVisitorTest >> testVisitDetect [
	| node tree |
	tree := self parseExpression: 'Object new'.
	node := OCGenericNodeVisitor  visit: tree detect: [ :n | n isMessage ].
	self  assert: node isMessage
]

{ #category : 'tests' }
OCGenericNodeVisitorTest >> testVisitDetectIfNone [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := OCGenericNodeVisitor visit: tree detect: [ :n | n isReturn ] ifNone: [ #none ].
	self assert: result equals: #none
]

{ #category : 'tests' }
OCGenericNodeVisitorTest >> testVisitNoneSatisfy [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := OCGenericNodeVisitor visit: tree noneSatisfy: [ :n | n isMessage ].
	self deny: result.
	result := OCGenericNodeVisitor visit: tree noneSatisfy: [ :n | n isReturn ].
	self assert: result
]

{ #category : 'tests' }
OCGenericNodeVisitorTest >> testVisitSelect [
	| result tree |
	tree := self parseExpression: 'Object new'.
	result := OCGenericNodeVisitor visit: tree select: [ :n | n isMessage ].
	self assert: result size equals: 1.
	self assert: result first selector equals: #new
]
