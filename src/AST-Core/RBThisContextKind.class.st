"
I am the strategy for thisContext
"
Class {
	#name : #RBThisContextKind,
	#superclass : #RBVariableKind,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBThisContextKind >> acceptVisitor: aProgramNodeVisitor [

	^ aProgramNodeVisitor visitThisContextNode: variable
]

{ #category : #testing }
RBThisContextKind >> isThisContext [ 

	^ true
]
