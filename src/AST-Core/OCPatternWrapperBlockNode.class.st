"
ASTPatternWrapperBlockNode allows further matching using a block after a node has been matched by a pattern node.

Instance Variables:
	wrappedNode	<ASTProgramNode>	The original pattern node to match
"
Class {
	#name : 'OCPatternWrapperBlockNode',
	#superclass : 'OCPatternBlockNode',
	#instVars : [
		'wrappedNode'
	],
	#category : 'AST-Core-Pattern',
	#package : 'AST-Core',
	#tag : 'Pattern'
}

{ #category : 'visiting' }
OCPatternWrapperBlockNode >> acceptVisitor: aProgramNodeVisitor [
	^aProgramNodeVisitor visitPatternWrapperBlockNode: self
]

{ #category : 'matching' }
OCPatternWrapperBlockNode >> copyInContext: aDictionary [
	"I don't know what this would mean, so ignore it."

	^ wrappedNode copyInContext: aDictionary
]

{ #category : 'matching' }
OCPatternWrapperBlockNode >> match: aNode inContext: aDictionary [
	(wrappedNode match: aNode inContext: aDictionary) ifFalse: [^false].
	^super match: aNode inContext: aDictionary
]

{ #category : 'accessing' }
OCPatternWrapperBlockNode >> precedence [
	^1
]

{ #category : 'accessing' }
OCPatternWrapperBlockNode >> sentMessages [
	^ wrappedNode sentMessages
]

{ #category : 'accessing' }
OCPatternWrapperBlockNode >> wrappedNode [
	^wrappedNode
]

{ #category : 'accessing' }
OCPatternWrapperBlockNode >> wrappedNode: aProgramNode [
	wrappedNode := aProgramNode.
	aProgramNode parent: self
]
