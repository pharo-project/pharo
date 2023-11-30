"
RBReplaceRule is the abstract superclass of all of the transforming rules. The rules change the source code by replacing the node that matches the rule. Subclasses implement different strategies for this replacement.

Subclasses must implement the following messages:
	matching
		foundMatchFor:

Instance Variables:
	verificationBlock	<BlockClosure>	Is evaluated with the matching node. This allows for further verification of a match beyond simple tree matching.


"
Class {
	#name : 'ASTReplaceRule',
	#superclass : 'ASTParseTreeRule',
	#instVars : [
		'verificationBlock'
	],
	#category : 'AST-Core-Matching',
	#package : 'AST-Core',
	#tag : 'Matching'
}

{ #category : 'class initialization' }
ASTReplaceRule class >> initialize [

	self deprecatedAliases: { #ASTReplaceRule }
]

{ #category : 'testing' }
ASTReplaceRule class >> isAbstract [

	^ self == ASTReplaceRule
]

{ #category : 'testing' }
ASTReplaceRule >> canMatch: aProgramNode [
	^verificationBlock value: aProgramNode
]

{ #category : 'matching' }
ASTReplaceRule >> foundMatchFor: aProgramNode [
	self subclassResponsibility
]

{ #category : 'initialization' }
ASTReplaceRule >> initialize [
	super initialize.
	verificationBlock := [:aNode | true]
]

{ #category : 'matching' }
ASTReplaceRule >> replace: aProgramNode with: newNode [
	aProgramNode replaceMethodSource: newNode
]

{ #category : 'private - searching' }
ASTReplaceRule >> searchFor: searchString replaceWith: replaceString [

	self subclassResponsibility
]

{ #category : 'private - searching' }
ASTReplaceRule >> searchFor: searchString replaceWith: replaceString when: aBlock [

	self subclassResponsibility
]

{ #category : 'private - searching' }
ASTReplaceRule >> searchForMethod: searchString replaceWith: replaceString [

	self subclassResponsibility
]

{ #category : 'private - searching' }
ASTReplaceRule >> searchForMethod: searchString replaceWith: replaceString when: aBlock [

	self subclassResponsibility
]

{ #category : 'private - searching' }
ASTReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceNode [

	self subclassResponsibility
]

{ #category : 'private - searching' }
ASTReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceString when: aBlock [

	self subclassResponsibility
]
