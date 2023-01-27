"
RBStringReplacement represents replacing source in the original method with a different string. These are used when reformatting code after a parse tree change has been made. Depending on the change, it may be possible to minimally change the parse tree without needing to format it.

Instance Variables:
	startPosition	<Integer>	the start position in the original source
	stopPosition	<Integer>	the end position in the original source
	string	<String>	replaces everything from the startPosition to the endPosition with this string


"
Class {
	#name : #RBStringReplacement,
	#superclass : #Object,
	#instVars : [
		'startPosition',
		'stopPosition',
		'string'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBStringReplacement class >> replaceFrom: startInteger to: stopInteger with: aString [
	^(self new)
		startPosition: startInteger;
		stopPosition: stopInteger;
		string: aString;
		yourself
]

{ #category : #accessing }
RBStringReplacement >> startPosition [
	^startPosition
]

{ #category : #initialization }
RBStringReplacement >> startPosition: anInteger [
	startPosition := anInteger
]

{ #category : #accessing }
RBStringReplacement >> stopPosition [
	^stopPosition
]

{ #category : #initialization }
RBStringReplacement >> stopPosition: anInteger [
	stopPosition := anInteger
]

{ #category : #accessing }
RBStringReplacement >> string [
	^string
]

{ #category : #initialization }
RBStringReplacement >> string: aString [
	string := aString
]
