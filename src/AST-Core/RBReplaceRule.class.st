"
RBReplaceRule is the abstract superclass of all of the transforming rules. The rules change the source code by replacing the node that matches the rule. Subclasses implement different strategies for this replacement.

Subclasses must implement the following messages:
	matching
		foundMatchFor:

Instance Variables:
	verificationBlock	<BlockClosure>	Is evaluated with the matching node. This allows for further verification of a match beyond simple tree matching.


"
Class {
	#name : #RBReplaceRule,
	#superclass : #RBParseTreeRule,
	#instVars : [
		'verificationBlock'
	],
	#category : #AST-Core-Matching
}

{ #category : #matching }
RBReplaceRule >> canMatch: aProgramNode [ 
	^verificationBlock value: aProgramNode
]

{ #category : #matching }
RBReplaceRule >> foundMatchFor: aProgramNode [ 
	self subclassResponsibility
]

{ #category : #initialization }
RBReplaceRule >> initialize [
	super initialize.
	verificationBlock := [:aNode | true]
]

{ #category : #matching }
RBReplaceRule >> replace: aProgramNode with: newNode [ 
	aProgramNode replaceMethodSource: newNode
]

{ #category : #matching }
RBReplaceRule >> searchFor: searchString replaceWith: replaceString [ 

	self subclassResponsibility
]

{ #category : #matching }
RBReplaceRule >> searchFor: searchString replaceWith: replaceString when: aBlock [ 

	self subclassResponsibility
]

{ #category : #matching }
RBReplaceRule >> searchForMethod: searchString replaceWith: replaceString [ 

	self subclassResponsibility
]

{ #category : #matching }
RBReplaceRule >> searchForMethod: searchString replaceWith: replaceString when: aBlock [ 

	self subclassResponsibility
]

{ #category : #matching }
RBReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceNode [ 

	self subclassResponsibility
]

{ #category : #matching }
RBReplaceRule >> searchForTree: aBRProgramNode replaceWith: replaceString when: aBlock [ 

	self subclassResponsibility
]
