Class {
	#name : #RBSequenceNodeTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBSequenceNodeTest >> testModifyParametersShouldNotModifyPeriods [

	| sequenceNode |
	sequenceNode := RBSequenceNode new.
	sequenceNode statements add: #example.
	self
		assertCollection: sequenceNode statements
		hasSameElements: { #example }.
	self assert: sequenceNode periods isEmpty.
	self assert: sequenceNode temporaries isEmpty
]

{ #category : #tests }
RBSequenceNodeTest >> testStop [
	| ast |
	ast := self parserClass parseMethod: 'method | temp |'.
	self assert: ast body stop equals: ast body temporaries last stop
]
