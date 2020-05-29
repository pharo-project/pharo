"
I am the strategy for temporaries
"
Class {
	#name : #RBTemporaryKind,
	#superclass : #RBVariableKind,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBTemporaryKind >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitTemporaryNode: variable
]

{ #category : #testing }
RBTemporaryKind >> isBlockVar [
	"true if a variable node is defined by a node whos parent is a block"

	^  (variable whoDefines: variable name) parent isBlock
]

{ #category : #testing }
RBTemporaryKind >> isTemp [ 
	
	^ true
]
