"
I am the strategy for self
"
Class {
	#name : #RBSelfKind,
	#superclass : #RBVariableKind,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBSelfKind >> acceptVisitor: aProgramNodeVisitor [

	^ aProgramNodeVisitor visitSelfNode: variable
]

{ #category : #testing }
RBSelfKind >> isSelf [ 
	
	^ true
]
