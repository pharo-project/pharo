Class {
	#name : 'ASTSequenceNodeTest',
	#superclass : 'RBParseTreeTest',
	#category : 'AST-Core-Tests-Nodes',
	#package : 'AST-Core-Tests',
	#tag : 'Nodes'
}

{ #category : 'tests' }
ASTSequenceNodeTest >> testModifyParametersShouldNotModifyPeriods [

	| sequenceNode |
	sequenceNode := ASTSequenceNode new.
	sequenceNode statements add: #example.
	self
		assertCollection: sequenceNode statements
		hasSameElements: { #example }.
	self assert: sequenceNode periods isEmpty.
	self assert: sequenceNode temporaries isEmpty
]

{ #category : 'tests' }
ASTSequenceNodeTest >> testStop [
	| ast |
	ast := self parserClass parseMethod: 'method | temp |'.
	self assert: ast body stop equals: ast body temporaries last stop
]
