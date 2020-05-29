"
I am the strategy for super
"
Class {
	#name : #RBSuperKind,
	#superclass : #RBVariableKind,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBSuperKind >> acceptVisitor: aProgramNodeVisitor [

	^ aProgramNodeVisitor visitSuperNode: variable
]

{ #category : #testing }
RBSuperKind >> isSuper [ 

	^ true
]
