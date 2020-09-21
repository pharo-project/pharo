"
Deprecated. See comment of RBTemporaryNode
"
Class {
	#name : #RBThisContextNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
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

{ #category : #initialization }
RBThisContextNode >> initialize [
  super initialize.
  variable := ThisContextVariable instance
]
