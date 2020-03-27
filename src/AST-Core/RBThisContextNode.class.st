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
		named: 'thisContext' start: 0;
		yourself.
]

{ #category : #visiting }
RBThisContextNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitThisContextNode: self
]

{ #category : #visiting }
RBThisContextNode >> isGlobal [
	^ false
]

{ #category : #visiting }
RBThisContextNode >> isInstance [
	^ false
]

{ #category : #visiting }
RBThisContextNode >> isThisContext [
	^ true
]
