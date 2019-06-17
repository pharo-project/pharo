"
I represent the specialized variable named 'thisContext'
"
Class {
	#name : #RBThisContextNode,
	#superclass : #RBVariableNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBThisContextNode class >> new [
	^super new 
		named: 'thisContext' start: nil;
		yourself.
]

{ #category : #visiting }
RBThisContextNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitThisContextNode: self
]
