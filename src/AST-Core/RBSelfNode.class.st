"
I am a specialized version for the 'self'
"
Class {
	#name : #RBSelfNode,
	#superclass : #RBVariableNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBSelfNode class >> new [
	^super new 
		named: 'self' start: 0;
		yourself.
]

{ #category : #visiting }
RBSelfNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSelfNode: self
]

{ #category : #testing }
RBSelfNode >> isSelf [
	^ true
]

{ #category : #testing }
RBSelfNode >> isSelfOrSuper [
	^ true
]
