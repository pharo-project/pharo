"
I am a specialized variable node for 'super'
"
Class {
	#name : #RBSuperNode,
	#superclass : #RBVariableNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBSuperNode >> acceptVisitor: aProgramNodeVisitor [ 
	^aProgramNodeVisitor visitSuperNode: self
]

{ #category : #testing }
RBSuperNode >> isSelfOrSuper [

	^ true
]

{ #category : #testing }
RBSuperNode >> isSuper [
	^ true
]
