"
ASTStringReplaceRule replaces a matched tree with another tree (which may include metavariable from the matching tree). This is a very succinct syntax for specifying most rewrites.

Instance Variables:
	replaceTree	<ASTProgramNode>	The tree to replace the matched tree with.


"
Class {
	#name : 'OCStringReplaceRule',
	#superclass : 'OCReplaceRule',
	#instVars : [
		'replaceTree'
	],
	#category : 'AST-Core-Matching',
	#package : 'AST-Core',
	#tag : 'Matching'
}

{ #category : 'instance creation' }
OCStringReplaceRule class >> searchFor: searchString replaceWith: replaceString [
	^self new searchFor: searchString replaceWith: replaceString
]

{ #category : 'instance creation' }
OCStringReplaceRule class >> searchFor: searchString replaceWith: replaceString when: aBlock [
	^self new
		searchFor: searchString
		replaceWith: replaceString
		when: aBlock
]

{ #category : 'instance creation' }
OCStringReplaceRule class >> searchForMethod: searchString replaceWith: replaceString [
	^(self new)
		searchForMethod: searchString replaceWith: replaceString;
		yourself
]

{ #category : 'instance creation' }
OCStringReplaceRule class >> searchForMethod: searchString replaceWith: replaceString when: aBlock [
	^self new
		searchForMethod: searchString
		replaceWith: replaceString
		when: aBlock
]

{ #category : 'instance creation' }
OCStringReplaceRule class >> searchForTree: aProgramNode replaceWith: replaceString [
	^self new searchForTree: aProgramNode replaceWith: replaceString
]

{ #category : 'instance creation' }
OCStringReplaceRule class >> searchForTree: aProgramNode replaceWith: replaceString when: aBlock [
	^self new
		searchForTree: aProgramNode
		replaceWith: replaceString
		when: aBlock
]

{ #category : 'matching' }
OCStringReplaceRule >> foundMatchFor: aProgramNode [
	| newNode |
	newNode := replaceTree copyInContext: self context.
	aProgramNode replaceMethodSource: newNode.
	newNode copyCommentsFrom: aProgramNode.
	^newNode
]

{ #category : 'initialization' }
OCStringReplaceRule >> methodReplaceString: replaceString [
	replaceTree := self parserClass parseRewriteMethod: replaceString
]

{ #category : 'initialization' }
OCStringReplaceRule >> replaceString: replaceString [
	replaceTree := self parserClass parseRewriteExpression: replaceString.
	searchTree isSequence = replaceTree isSequence
		ifFalse: [ searchTree isSequence
				ifTrue: [ replaceTree := OCSequenceNode statements: (Array with: replaceTree) ]
				ifFalse: [ searchTree := OCSequenceNode statements: (Array with: searchTree) ] ]
]

{ #category : 'private - searching' }
OCStringReplaceRule >> searchFor: searchString replaceWith: replaceString [
	self searchString: searchString.
	self replaceString: replaceString
]

{ #category : 'private - searching' }
OCStringReplaceRule >> searchFor: searchString replaceWith: replaceString when: aBlock [
	self searchFor: searchString replaceWith: replaceString.
	verificationBlock := aBlock
]

{ #category : 'private - searching' }
OCStringReplaceRule >> searchForMethod: searchString replaceWith: replaceString [
	self methodSearchString: searchString.
	self methodReplaceString: replaceString
]

{ #category : 'private - searching' }
OCStringReplaceRule >> searchForMethod: searchString replaceWith: replaceString when: aBlock [
	self searchForMethod: searchString replaceWith: replaceString.
	verificationBlock := aBlock
]

{ #category : 'private - searching' }
OCStringReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceNode [
	searchTree := aBRProgramNode.
	replaceTree := replaceNode
]

{ #category : 'private - searching' }
OCStringReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceString when: aBlock [
	self searchForTree: aBRProgramNode replaceWith: replaceString.
	verificationBlock := aBlock
]
