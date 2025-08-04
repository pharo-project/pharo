"
ASTReplaceRule is the abstract superclass of all of the transforming rules. The rules change the source code by replacing the node that matches the rule. Subclasses implement different strategies for this replacement.

Subclasses must implement the following messages:
	matching
		foundMatchFor:

Instance Variables:
	verificationBlock	<BlockClosure>	Is evaluated with the matching node. This allows for further verification of a match beyond simple tree matching.


"
Class {
	#name : 'OCReplaceRule',
	#superclass : 'OCParseTreeRule',
	#instVars : [
		'verificationBlock'
	],
	#category : 'AST-Core-Matching',
	#package : 'AST-Core',
	#tag : 'Matching'
}

{ #category : 'testing' }
OCReplaceRule class >> isAbstract [

	^ self == OCReplaceRule
]

{ #category : 'testing' }
OCReplaceRule >> canMatch: aProgramNode [
	^verificationBlock value: aProgramNode
]

{ #category : 'matching' }
OCReplaceRule >> foundMatchFor: aProgramNode [
	self subclassResponsibility
]

{ #category : 'initialization' }
OCReplaceRule >> initialize [
	super initialize.
	verificationBlock := [:aNode | true]
]

{ #category : 'matching' }
OCReplaceRule >> replace: aProgramNode with: newNode [
	aProgramNode replaceMethodSource: newNode
]

{ #category : 'private - searching' }
OCReplaceRule >> searchFor: searchString replaceWith: replaceString [

	self subclassResponsibility
]

{ #category : 'private - searching' }
OCReplaceRule >> searchFor: searchString replaceWith: replaceString when: aBlock [

	self subclassResponsibility
]

{ #category : 'private - searching' }
OCReplaceRule >> searchForMethod: searchString replaceWith: replaceString [

	self subclassResponsibility
]

{ #category : 'private - searching' }
OCReplaceRule >> searchForMethod: searchString replaceWith: replaceString when: aBlock [

	self subclassResponsibility
]

{ #category : 'private - searching' }
OCReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceNode [

	self subclassResponsibility
]

{ #category : 'private - searching' }
OCReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceString when: aBlock [

	self subclassResponsibility
]
