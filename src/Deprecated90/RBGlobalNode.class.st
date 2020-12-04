"
Deprecated. See comment of RBTemporaryNode
"
Class {
	#name : #RBGlobalNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
}

{ #category : #visiting }
RBGlobalNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitGlobalNode: self
]
