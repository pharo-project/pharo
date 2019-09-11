Class {
	#name : #RBParseErrorNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBParseErrorNodeTest >> testErrorNodeStart [
	"check that the start of the error node is correct"

	| ast |
	ast := self parserClass parseFaultyMethod: 'method ( 1 + 3'.
	"the start should be much earlier then the end of the method"
	self assert: ast body statements first start < 14
]
