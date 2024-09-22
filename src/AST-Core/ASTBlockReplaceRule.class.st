"
RBBlockReplaceRule replaces the matching node by the result of evaluating replaceBlock. This allows arbitrary computation to come up with a replacement.

Instance Variables:
	replaceBlock	<BlockClosure>	The block that returns the node to replace the matching node.

"
Class {
	#name : 'ASTBlockReplaceRule',
	#superclass : 'ASTReplaceRule',
	#instVars : [
		'replaceBlock'
	],
	#category : 'AST-Core-Matching',
	#package : 'AST-Core',
	#tag : 'Matching'
}

{ #category : 'instance creation' }
ASTBlockReplaceRule class >> searchFor: searchString replaceWith: replaceBlock [
	^self new searchFor: searchString replaceWith: replaceBlock
]

{ #category : 'instance creation' }
ASTBlockReplaceRule class >> searchFor: searchString replaceWith: replaceBlock when: aBlock [
	^self new
		searchFor: searchString
		replaceWith: replaceBlock
		when: aBlock
]

{ #category : 'instance creation' }
ASTBlockReplaceRule class >> searchForMethod: searchString replaceWith: replaceBlock [
	^self new searchForMethod: searchString replaceWith: replaceBlock
]

{ #category : 'instance creation' }
ASTBlockReplaceRule class >> searchForMethod: searchString replaceWith: replaceBlock when: aBlock [
	^self new
		searchForMethod: searchString
		replaceWith: replaceBlock
		when: aBlock
]

{ #category : 'instance creation' }
ASTBlockReplaceRule class >> searchForTree: aProgramNode replaceWith: replaceBlock [
	^self new searchForTree: aProgramNode replaceWith: replaceBlock
]

{ #category : 'instance creation' }
ASTBlockReplaceRule class >> searchForTree: aProgramNode replaceWith: replaceBlock when: aBlock [
	^self new
		searchForTree: aProgramNode
		replaceWith: replaceBlock
		when: aBlock
]

{ #category : 'matching' }
ASTBlockReplaceRule >> foundMatchFor: aProgramNode [
	| newNode |
	newNode := replaceBlock value: aProgramNode.
	aProgramNode replaceMethodSource: newNode.
	^newNode
]

{ #category : 'initialization' }
ASTBlockReplaceRule >> initialize [
	super initialize.
	replaceBlock := [:aNode | aNode]
]

{ #category : 'private - searching' }
ASTBlockReplaceRule >> searchFor: searchString replaceWith: aBlock [
	self searchString: searchString.
	replaceBlock := aBlock
]

{ #category : 'private - searching' }
ASTBlockReplaceRule >> searchFor: searchString replaceWith: replBlock when: verifyBlock [
	self searchFor: searchString replaceWith: replBlock.
	verificationBlock := verifyBlock
]

{ #category : 'private - searching' }
ASTBlockReplaceRule >> searchForMethod: searchString replaceWith: aBlock [
	self methodSearchString: searchString.
	replaceBlock := aBlock
]

{ #category : 'private - searching' }
ASTBlockReplaceRule >> searchForMethod: searchString replaceWith: replBlock when: verifyBlock [
	self searchForMethod: searchString replaceWith: replBlock.
	verificationBlock := verifyBlock
]

{ #category : 'private - searching' }
ASTBlockReplaceRule >> searchForTree: aBRProgramNode replaceWith: aBlock [
	searchTree := aBRProgramNode.
	replaceBlock := aBlock
]

{ #category : 'private - searching' }
ASTBlockReplaceRule >> searchForTree: aBRProgramNode replaceWith: replBlock when: verifyBlock [
	self searchForTree: aBRProgramNode replaceWith: replBlock.
	verificationBlock := verifyBlock
]
