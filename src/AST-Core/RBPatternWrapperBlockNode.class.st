"
RBPatternWrapperBlockNode allows further matching using a block after a node has been matched by a pattern node.

Instance Variables:
	wrappedNode	<RBProgramNode>	The original pattern node to match
"
Class {
	#name : #RBPatternWrapperBlockNode,
	#superclass : #RBPatternBlockNode,
	#instVars : [
		'wrappedNode'
	],
	#category : #'AST-Core-Pattern'
}

{ #category : #visiting }
RBPatternWrapperBlockNode >> acceptVisitor: aProgramNodeVisitor [
	^aProgramNodeVisitor visitPatternWrapperBlockNode: self
]

{ #category : #matching }
RBPatternWrapperBlockNode >> copyInContext: aDictionary [
	"I don't know what this would mean, so ignore it."

	^ wrappedNode copyInContext: aDictionary
]

{ #category : #matching }
RBPatternWrapperBlockNode >> match: aNode inContext: aDictionary [
	(wrappedNode match: aNode inContext: aDictionary) ifFalse: [^false].
	^super match: aNode inContext: aDictionary
]

{ #category : #accessing }
RBPatternWrapperBlockNode >> precedence [
	^1
]

{ #category : #accessing }
RBPatternWrapperBlockNode >> sentMessages [
	^ wrappedNode sentMessages
]

{ #category : #accessing }
RBPatternWrapperBlockNode >> wrappedNode [
	^wrappedNode
]

{ #category : #accessing }
RBPatternWrapperBlockNode >> wrappedNode: aRBProgramNode [
	wrappedNode := aRBProgramNode.
	aRBProgramNode parent: self
]
