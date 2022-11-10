Class {
	#name : #RBBlockNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBBlockNodeTest >> testHasBlockReturnRecursive [

	self deny: [  ] sourceNode hasReturn.
	self deny: [ [  ] ] sourceNode hasReturn.
	self assert: [ ^ 1 ] sourceNode hasReturn.
	self assert: [ [ ^ 1 ] ] sourceNode hasReturn
]
