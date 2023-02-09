"
I am testing AST evaluation 
"
Class {
	#name : #ASTEvaluationTest,
	#superclass : #TestCase,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
ASTEvaluationTest >> testEvaluate [
	self assert: (RBLiteralNode value: 5) evaluate equals: 5.
	self assert: (RBMessageNode receiver: 5 asLiteralNode selector: #class ) evaluate equals: 5 class
]

{ #category : #tests }
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

	"thisContext is not the thisContext of this method though... it is the context of the evaluating doit"
	node := RBVariableNode thisContextNode.
	self deny: (node evaluateForContext: thisContext) equals: thisContext.

	"reading ivars works, too"
	node := RBVariableNode named: 'testSelector'.
	self assert: (node evaluateForContext: thisContext) equals: #testEvaluateForContext
]

{ #category : #tests }
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
