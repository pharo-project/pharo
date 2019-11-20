"
I am a specific variable node for method and block arguments.

Parsing a method or block code will just use RBVariableNodes for block/method arguments until we call doSemanticAnalysis on the method node.
"
Class {
	#name : #RBArgumentNode,
	#superclass : #RBVariableNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBArgumentNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitArgumentNode: self
]

{ #category : #converting }
RBArgumentNode >> adaptToSemanticNode [
	" I can't provide more semantics "
]

{ #category : #testing }
RBArgumentNode >> isArgument [
	^ true
]

{ #category : #testing }
RBArgumentNode >> isBlockVar [
	"true if a variable node is defined by a block node"

	^ (self whoDefines: self name) isBlock
]
