"
I am the strategy for arguments
"
Class {
	#name : #RBArgumentKind,
	#superclass : #RBVariableKind,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBArgumentKind >> acceptVisitor: aProgramNodeVisitor [

	^ aProgramNodeVisitor visitArgumentNode: variable
]

{ #category : #testing }
RBArgumentKind >> isArgument [
	^ true
]

{ #category : #testing }
RBArgumentKind >> isBlockVar [
	"true if a variable node is defined by a block node"

	^ (variable whoDefines: variable name) isBlock
]
