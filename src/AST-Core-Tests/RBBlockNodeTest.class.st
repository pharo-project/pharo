Class {
	#name : #RBBlockNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBBlockNodeTest >> testHasReturn [

	self deny: [  ] sourceNode hasReturn.
	self deny: [ [  ] ] sourceNode hasReturn.
	self assert: [ ^ 1 ] sourceNode hasReturn.
	self assert: [ [ ^ 1 ] ] sourceNode hasReturn.
	self assert: (self class>>#testHasReturn) ast hasReturn.
	self deny: (Object>>#halt) ast hasReturn
]
