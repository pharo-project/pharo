"
I am the strategy for instance variables
"
Class {
	#name : #RBInstanceVariableKind,
	#superclass : #RBVariableKind,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBInstanceVariableKind >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitInstanceVariableNode: variable
]

{ #category : #testing }
RBInstanceVariableKind >> isInstance [ 

	^ true
]
