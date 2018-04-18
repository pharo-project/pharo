Class {
	#name : #ASTEvaluationTest,
	#superclass : #TestCase,
	#category : #'AST-Tests-Core'
}

{ #category : #tests }
ASTEvaluationTest >> testeEvaluate [
	self assert: (RBLiteralNode value: 5) evaluate equals: 5
]

{ #category : #tests }
ASTEvaluationTest >> testeEvaluateForContext [
	| varForTesting node |
	varForTesting := 4@5.
	node := thisContext method variableNodes first.
	self assert: (node evaluateForContext: thisContext) equals: varForTesting.
]

{ #category : #tests }
ASTEvaluationTest >> testeEvaluateForReceiver [
	| receiver node |
	receiver := 4@5.
	node := (receiver class>>#x) variableNodes first.
	self assert: (node evaluateForReceiver: receiver) equals: 4.
]

{ #category : #tests }
ASTEvaluationTest >> testeEvaluateForReceiverSelf [
	| receiver node |
	receiver := 4@5.
	node := RBVariableNode named: 'self'.
	self assert: (node evaluateForReceiver: receiver) equals: receiver.
]

{ #category : #tests }
ASTEvaluationTest >> testeEvaluateForReceiverSelfSuper [
	| receiver node |
	receiver := 4@5.
	node := RBVariableNode named: 'self'.
	self assert: (node evaluateForReceiver: receiver) equals: receiver.
	node := RBVariableNode named: 'super'.
	self assert: (node evaluateForReceiver: receiver) equals: receiver.
]
