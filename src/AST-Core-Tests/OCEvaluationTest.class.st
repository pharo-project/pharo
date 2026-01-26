"
I am testing AST evaluation 
"
Class {
	#name : 'OCEvaluationTest',
	#superclass : 'TestCase',
	#category : 'AST-Core-Tests-Nodes',
	#package : 'AST-Core-Tests',
	#tag : 'Nodes'
}

{ #category : 'tests' }
OCEvaluationTest >> testEvaluate [
	self assert: (OCLiteralNode value: 5) evaluate equals: 5.
	self assert: (OCMessageNode receiver: 5 asLiteralNode selector: #class ) evaluate equals: 5 class
]

{ #category : 'tests' }
OCEvaluationTest >> testEvaluateForContext [
	| varForTesting node |
	varForTesting := 4@5.

	"first we test if we can read the temp varForTesting"
	node := thisContext method variableNodes first.
	self assert: (node evaluateForContext: thisContext) equals: varForTesting.

	"lets check self, super"
	node := OCVariableNode selfNode.
	self assert: (node evaluateForContext: thisContext) equals: thisContext receiver.
	node := OCVariableNode superNode.
	self assert: (node evaluateForContext: thisContext) equals: thisContext receiver.

	node := OCVariableNode thisContextNode.
	self assert: (node evaluateForContext: thisContext) equals: thisContext.

	"reading ivars works, too"
	node := OCVariableNode named: 'testSelector'.
	self assert: (node evaluateForContext: thisContext) equals: #testEvaluateForContext
]

{ #category : 'tests' }
OCEvaluationTest >> testEvaluateForReceiver [
	| receiver node |
	receiver := 4@5.
	node := (receiver class>>#x) variableNodes first.
	self assert: (node evaluateForReceiver: receiver) equals: 4.
	node := OCVariableNode selfNode.
	self assert: (node evaluateForReceiver: receiver) equals: receiver.
	node := OCVariableNode superNode.
	self assert: (node evaluateForReceiver: receiver) equals: receiver
]
