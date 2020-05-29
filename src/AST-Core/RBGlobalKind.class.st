"
I am the strategy for globals
"
Class {
	#name : #RBGlobalKind,
	#superclass : #RBVariableKind,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBGlobalKind >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitGlobalNode: variable
]

{ #category : #testing }
RBGlobalKind >> isGlobal [

	^ true
]
