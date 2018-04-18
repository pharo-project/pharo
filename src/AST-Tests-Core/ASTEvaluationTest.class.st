"
I am testing AST evaluation 
"
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
	node := RBVariableNode named: 'self'.
	self assert: (node evaluateForContext: thisContext) equals: thisContext receiver.
	node := RBVariableNode named: 'testSelector'.
	self assert: (node evaluateForContext: thisContext) equals: #testeEvaluateForContext.
]

{ #category : #tests }
ASTEvaluationTest >> testeEvaluateForReceiver [
	| receiver node |
	receiver := 4@5.
	node := (receiver class>>#x) variableNodes first.
	self assert: (node evaluateForReceiver: receiver) equals: 4.
	node := RBVariableNode named: 'self'.
	self assert: (node evaluateForReceiver: receiver) equals: receiver.
	node := RBVariableNode named: 'super'.
	self assert: (node evaluateForReceiver: receiver) equals: receiver.
]
