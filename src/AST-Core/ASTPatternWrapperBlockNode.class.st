"
ASTPatternWrapperBlockNode allows further matching using a block after a node has been matched by a pattern node.

Instance Variables:
	wrappedNode	<ASTProgramNode>	The original pattern node to match
"
Class {
	#name : 'ASTPatternWrapperBlockNode',
	#superclass : 'ASTPatternBlockNode',
	#instVars : [
		'wrappedNode'
	],
	#category : 'AST-Core-Pattern',
	#package : 'AST-Core',
	#tag : 'Pattern'
}

{ #category : 'visiting' }
ASTPatternWrapperBlockNode >> acceptVisitor: aProgramNodeVisitor [
	^aProgramNodeVisitor visitPatternWrapperBlockNode: self
]

{ #category : 'matching' }
ASTPatternWrapperBlockNode >> copyInContext: aDictionary [
	"I don't know what this would mean, so ignore it."

	^ wrappedNode copyInContext: aDictionary
]

{ #category : 'matching' }
ASTPatternWrapperBlockNode >> match: aNode inContext: aDictionary [
	(wrappedNode match: aNode inContext: aDictionary) ifFalse: [^false].
	^super match: aNode inContext: aDictionary
]

{ #category : 'accessing' }
ASTPatternWrapperBlockNode >> precedence [
	^1
]

{ #category : 'accessing' }
ASTPatternWrapperBlockNode >> sentMessages [
	^ wrappedNode sentMessages
]

{ #category : 'accessing' }
ASTPatternWrapperBlockNode >> wrappedNode [
	^wrappedNode
]

{ #category : 'accessing' }
ASTPatternWrapperBlockNode >> wrappedNode: aProgramNode [
	wrappedNode := aProgramNode.
	aProgramNode parent: self
]
