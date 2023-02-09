"
RBSearchRule is a parse tree rule that simply searches for matches to the rule. Every time a match is found, answerBlock is evaluated with the node that matches and the current answer. This two-argument approach allows a collection to be formed from all of the matches (Think inject:into:).

Instance Variables:
	answerBlock	<BlockClosure>	Block to evaluate with the matching node and the current answer.


"
Class {
	#name : #RBSearchRule,
	#superclass : #RBParseTreeRule,
	#instVars : [
		'answerBlock'
	],
	#category : #'AST-Core-Matching'
}

{ #category : #'instance creation' }
RBSearchRule class >> searchFor: aString thenDo: aBlock [
	^self new searchFor: aString thenDo: aBlock
]

{ #category : #'instance creation' }
RBSearchRule class >> searchForMethod: aString thenDo: aBlock [
	^self new searchForMethod: aString thenDo: aBlock
]

{ #category : #'instance creation' }
RBSearchRule class >> searchForTree: aBRProgramNode thenDo: aBlock [
	^self new searchForTree: aBRProgramNode thenDo: aBlock
]

{ #category : #testing }
RBSearchRule >> canMatch: aProgramNode [
	owner answer: (answerBlock value: aProgramNode value: owner answer).
	^true
]

{ #category : #'private - searching' }
RBSearchRule >> searchFor: aString thenDo: aBlock [
	self searchString: aString.
	answerBlock := aBlock
]

{ #category : #'private - searching' }
RBSearchRule >> searchForMethod: aString thenDo: aBlock [
	self methodSearchString: aString.
	answerBlock := aBlock
]

{ #category : #'private - searching' }
RBSearchRule >> searchForTree: aBRProgramNode thenDo: aBlock [
	searchTree := aBRProgramNode.
	answerBlock := aBlock
]
