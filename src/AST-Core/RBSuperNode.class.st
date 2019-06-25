"
I am a specialized variable node for 'super'
"
Class {
	#name : #RBSuperNode,
	#superclass : #RBVariableNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBSuperNode class >> new [
	^super new 
		named: 'super' start: 0;
		yourself.
]

{ #category : #visiting }
RBSuperNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSuperNode: self
]

{ #category : #testing }
RBSuperNode >> isSelfOrSuper [

	^ true
]

{ #category : #testing }
RBSuperNode >> isSuper [
	^ true
]
