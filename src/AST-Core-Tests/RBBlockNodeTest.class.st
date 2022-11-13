Class {
	#name : #RBBlockNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBBlockNodeTest >> testHasNonLocalReturn [

	self deny: [  ] sourceNode hasNonLocalReturn.
	self deny: [ [  ] ] sourceNode hasNonLocalReturn.
	self assert: [ ^ 1 ] sourceNode hasNonLocalReturn.
	self assert: [ [ ^ 1 ] ] sourceNode hasNonLocalReturn.
	self assert: (self class>>#testHasNonLocalReturn) ast hasNonLocalReturn.
	"return on the level of the method is a local return"
	self deny: (Object>>#yourself) ast hasNonLocalReturn.
	self deny: (Object>>#halt) ast hasNonLocalReturn
]
