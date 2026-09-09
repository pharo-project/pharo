Class {
	#name : 'OCSequenceNodeTest',
	#superclass : 'OCParseTreeTest',
	#category : 'AST-Core-Tests-Nodes',
	#package : 'AST-Core-Tests',
	#tag : 'Nodes'
}

{ #category : 'tests' }
OCSequenceNodeTest >> testModifyParametersShouldNotModifyPeriods [

	| sequenceNode |
	sequenceNode := OCSequenceNode new.
	sequenceNode statements add: #example.
	self
		assertCollection: sequenceNode statements
		hasSameElements: { #example }.
	self assert: sequenceNode periods isEmpty.
	self assert: sequenceNode temporaries isEmpty
]

{ #category : 'tests' }
OCSequenceNodeTest >> testStop [
	| ast |
	ast := self parserClass parseMethod: 'method | temp |'.
	self assert: ast body stop equals: ast body temporaries last stop
]
