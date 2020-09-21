"
Deprecated. See comment of RBTemporaryNode
"
Class {
	#name : #RBSelfNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
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

{ #category : #initialization }
RBSelfNode >> initialize [
  super initialize.
  variable := SelfVariable instance
]
