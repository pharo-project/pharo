"
I am testing AST evaluation 
"
Class {
	#name : 'ASTEvaluationTest',
	#superclass : 'TestCase',
	#category : 'AST-Core-Tests-Nodes',
	#package : 'AST-Core-Tests',
	#tag : 'Nodes'
}

{ #category : 'tests' }
ASTEvaluationTest >> testEvaluate [
	self assert: (ASTLiteralNode value: 5) evaluate equals: 5.
	self assert: (RBMessageNode receiver: 5 asLiteralNode selector: #class ) evaluate equals: 5 class
]

{ #category : 'tests' }
ASTEvaluationTest >> testEvaluateForContext [
	| varForTesting node |
	varForTesting := 4@5.

	"first we test if we can read the temp varForTesting"
	node := thisContext method variableNodes first.
	self assert: (node evaluateForContext: thisContext) equals: varForTesting.

	"lets check self, super"
	node := RBVariableNode selfNode.
	self assert: (node evaluateForContext: thisContext) equals: thisContext receiver.
	node := RBVariableNode superNode.
	self assert: (node evaluateForContext: thisContext) equals: thisContext receiver.

	node := RBVariableNode thisContextNode.
	self assert: (node evaluateForContext: thisContext) equals: thisContext.

	"reading ivars works, too"
	node := RBVariableNode named: 'testSelector'.
	self assert: (node evaluateForContext: thisContext) equals: #testEvaluateForContext
]

{ #category : 'tests' }
ASTEvaluationTest >> testEvaluateForReceiver [
	| receiver node |
	receiver := 4@5.
	node := (receiver class>>#x) variableNodes first.
	self assert: (node evaluateForReceiver: receiver) equals: 4.
	node := RBVariableNode selfNode.
	self assert: (node evaluateForReceiver: receiver) equals: receiver.
	node := RBVariableNode superNode.
	self assert: (node evaluateForReceiver: receiver) equals: receiver
]
