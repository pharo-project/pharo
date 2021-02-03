"
Deprecated. See comment of RBTemporaryNode
"
Class {
	#name : #RBArgumentNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
}

{ #category : #visiting }
RBArgumentNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitArgumentNode: self
]

{ #category : #generated }
RBArgumentNode >> isArgument [

	self
		deprecated: 'Use #isArgumentVariable instead'
		transformWith: '`@rec isArgument' -> '`@rec isArgumentVariable'.
	^ self isArgumentVariable
]
