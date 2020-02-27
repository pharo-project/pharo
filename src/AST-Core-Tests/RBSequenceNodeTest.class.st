Class {
	#name : #RBSequenceNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBSequenceNodeTest >> testStop [
	| ast |
	ast := self parserClass parseMethod: 'method | temp |'.
	self assert: ast body stop equals: ast body temporaries last stop
]
