"
Deprecated. See comment of RBTemporaryNode
"
Class {
	#name : #RBInstanceVariableNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
}

{ #category : #visting }
RBInstanceVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitInstanceVariableNode: self
]
