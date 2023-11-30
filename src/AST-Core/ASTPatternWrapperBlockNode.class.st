"
RBPatternWrapperBlockNode allows further matching using a block after a node has been matched by a pattern node.

Instance Variables:
	wrappedNode	<RBProgramNode>	The original pattern node to match
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

{ #category : 'class initialization' }
ASTPatternWrapperBlockNode class >> initialize [

	self deprecatedAliases: { #ASTPatternWrapperBlockNode }
]

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
ASTPatternWrapperBlockNode >> wrappedNode: aRBProgramNode [
	wrappedNode := aRBProgramNode.
	aRBProgramNode parent: self
]
