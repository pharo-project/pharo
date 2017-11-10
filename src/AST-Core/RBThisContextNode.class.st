"
I represent the specialized variable named 'thisContext'
"
Class {
	#name : #RBThisContextNode,
	#superclass : #RBVariableNode,
	#category : #AST-Core-Nodes
}

{ #category : #visiting }
RBThisContextNode >> acceptVisitor: aProgramNodeVisitor [ 
	^aProgramNodeVisitor visitThisContextNode: self
]
