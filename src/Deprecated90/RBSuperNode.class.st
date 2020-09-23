"
Deprecated. See comment of RBTemporaryNode
"
Class {
	#name : #RBSuperNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
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

{ #category : #initialization }
RBSuperNode >> initialize [
  super initialize.
  variable := SuperVariable instance
]
