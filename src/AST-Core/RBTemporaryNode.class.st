"
I am a specialized variable node for temporary variables.

Parsing a method or block code will just use RBVariableNodes for block/method arguments until we call doSemanticAnalysis on the method node.
"
Class {
	#name : #RBTemporaryNode,
	#superclass : #RBVariableNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBTemporaryNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitTemporaryNode: self
]

{ #category : #testing }
RBTemporaryNode >> isTemp [
	^ true
]
