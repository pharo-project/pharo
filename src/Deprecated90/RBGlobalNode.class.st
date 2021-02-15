"
Deprecated. See comment of RBTemporaryNode
"
Class {
	#name : #RBGlobalNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
}

{ #category : #visiting }
RBGlobalNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitGlobalNode: self
]

{ #category : #generated }
RBGlobalNode >> isGlobal [

	self
		deprecated: 'Use #isGlobalVariable instead'
		transformWith: '`@rec isGlobal' -> '`@rec isGlobalVariable'.
	^ self isGlobalVariable
]
