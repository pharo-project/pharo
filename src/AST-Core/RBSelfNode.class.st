"
I am a specialized version for the 'self'
"
Class {
	#name : #RBSelfNode,
	#superclass : #RBVariableNode,
	#category : #'AST-Core-Nodes'
}

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
