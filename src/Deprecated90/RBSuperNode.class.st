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

{ #category : #generated }
RBSuperNode >> isSuper [

	self
		deprecated: 'Use #isSuperVariable instead'
		transformWith: '`@rec isSuper' -> '`@rec isSuperVariable'.
	^ self isSuperVariable
]
