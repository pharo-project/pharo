"
I am a specialized variable node for instance variables.

Just like RBGlobalNode, RBInstanceVariableNodes are only used in an RBMethodNode after we do a semantic analyzing with a proper compilation context to deduce the class for which this variable is an instance variable.
"
Class {
	#name : #RBInstanceVariableNode,
	#superclass : #RBVariableNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #visting }
RBInstanceVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitInstanceVariableNode: self
]

{ #category : #testing }
RBInstanceVariableNode >> isInstance [
	^true
]
