"
RBStringReplaceRule replaces a matched tree with another tree (which may include metavariable from the matching tree). This is a very succinct syntax for specifying most rewrites.

Instance Variables:
	replaceTree	<RBProgramNode>	The tree to replace the matched tree with.


"
Class {
	#name : #RBStringReplaceRule,
	#superclass : #RBReplaceRule,
	#instVars : [
		'replaceTree'
	],
	#category : #'AST-Core-Matching'
}

{ #category : #'instance creation' }
RBStringReplaceRule class >> searchFor: searchString replaceWith: replaceString [
	^self new searchFor: searchString replaceWith: replaceString
]

{ #category : #'instance creation' }
RBStringReplaceRule class >> searchFor: searchString replaceWith: replaceString when: aBlock [
	^self new
		searchFor: searchString
		replaceWith: replaceString
		when: aBlock
]

{ #category : #'instance creation' }
RBStringReplaceRule class >> searchForMethod: searchString replaceWith: replaceString [
	^(self new)
		searchForMethod: searchString replaceWith: replaceString;
		yourself
]

{ #category : #'instance creation' }
RBStringReplaceRule class >> searchForMethod: searchString replaceWith: replaceString when: aBlock [
	^self new
		searchForMethod: searchString
		replaceWith: replaceString
		when: aBlock
]

{ #category : #'instance creation' }
RBStringReplaceRule class >> searchForTree: aRBProgramNode replaceWith: replaceString [
	^self new searchForTree: aRBProgramNode replaceWith: replaceString
]

{ #category : #'instance creation' }
RBStringReplaceRule class >> searchForTree: aRBProgramNode replaceWith: replaceString when: aBlock [
	^self new
		searchForTree: aRBProgramNode
		replaceWith: replaceString
		when: aBlock
]

{ #category : #matching }
RBStringReplaceRule >> foundMatchFor: aProgramNode [
	| newNode |
	newNode := replaceTree copyInContext: self context.
	aProgramNode replaceMethodSource: newNode.
	newNode copyCommentsFrom: aProgramNode.
	^newNode
]

{ #category : #initialization }
RBStringReplaceRule >> methodReplaceString: replaceString [
	replaceTree := self parserClass parseRewriteMethod: replaceString
]

{ #category : #initialization }
RBStringReplaceRule >> replaceString: replaceString [
	replaceTree := self parserClass parseRewriteExpression: replaceString.
	searchTree isSequence = replaceTree isSequence
		ifFalse: [ searchTree isSequence
				ifTrue: [ replaceTree := RBSequenceNode statements: (Array with: replaceTree) ]
				ifFalse: [ searchTree := RBSequenceNode statements: (Array with: searchTree) ] ]
]

{ #category : #'private - searching' }
RBStringReplaceRule >> searchFor: searchString replaceWith: replaceString [
	self searchString: searchString.
	self replaceString: replaceString
]

{ #category : #'private - searching' }
RBStringReplaceRule >> searchFor: searchString replaceWith: replaceString when: aBlock [
	self searchFor: searchString replaceWith: replaceString.
	verificationBlock := aBlock
]

{ #category : #'private - searching' }
RBStringReplaceRule >> searchForMethod: searchString replaceWith: replaceString [
	self methodSearchString: searchString.
	self methodReplaceString: replaceString
]

{ #category : #'private - searching' }
RBStringReplaceRule >> searchForMethod: searchString replaceWith: replaceString when: aBlock [
	self searchForMethod: searchString replaceWith: replaceString.
	verificationBlock := aBlock
]

{ #category : #'private - searching' }
RBStringReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceNode [
	searchTree := aBRProgramNode.
	replaceTree := replaceNode
]

{ #category : #'private - searching' }
RBStringReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceString when: aBlock [
	self searchForTree: aBRProgramNode replaceWith: replaceString.
	verificationBlock := aBlock
]
