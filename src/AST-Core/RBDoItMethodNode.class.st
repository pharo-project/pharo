"
RBDoItMethodNode is the node that represents AST of DoIt expressions.
It allows to distinguish between ASTs of real methods and DoIts. 

- aMethodNode isDoIt

"
Class {
	#name : #RBDoItMethodNode,
	#superclass : #RBMethodNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #initialization }
RBDoItMethodNode >> initialize [
	super initialize.

	selector := #DoIt
]

{ #category : #testing }
RBDoItMethodNode >> isDoIt [
	^true
]
