"
Deprecated. See comment of RBTemporaryNode
"
Class {
	#name : #RBInstanceVariableNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
}

{ #category : #visting }
RBInstanceVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitInstanceVariableNode: self
]

{ #category : #generated }
RBInstanceVariableNode >> isInstance [

	self
		deprecated: 'Use #isInstanceVariable instead'
		transformWith: '`@rec isInstance' -> '`@rec isInstanceVariable'.
	^ self isInstanceVariable
]
