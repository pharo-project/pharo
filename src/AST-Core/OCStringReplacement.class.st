"
ASTStringReplacement represents replacing source in the original method with a different string. These are used when reformatting code after a parse tree change has been made. Depending on the change, it may be possible to minimally change the parse tree without needing to format it.

Instance Variables:
	startPosition	<Integer>	the start position in the original source
	stopPosition	<Integer>	the end position in the original source
	string	<String>	replaces everything from the startPosition to the endPosition with this string


"
Class {
	#name : 'OCStringReplacement',
	#superclass : 'Object',
	#instVars : [
		'startPosition',
		'stopPosition',
		'string'
	],
	#category : 'AST-Core-Matching',
	#package : 'AST-Core',
	#tag : 'Matching'
}

{ #category : 'instance creation' }
OCStringReplacement class >> replaceFrom: startInteger to: stopInteger with: aString [
	^(self new)
		startPosition: startInteger;
		stopPosition: stopInteger;
		string: aString;
		yourself
]

{ #category : 'accessing' }
OCStringReplacement >> startPosition [
	^startPosition
]

{ #category : 'initialization' }
OCStringReplacement >> startPosition: anInteger [
	startPosition := anInteger
]

{ #category : 'accessing' }
OCStringReplacement >> stopPosition [
	^stopPosition
]

{ #category : 'initialization' }
OCStringReplacement >> stopPosition: anInteger [
	stopPosition := anInteger
]

{ #category : 'accessing' }
OCStringReplacement >> string [
	^string
]

{ #category : 'initialization' }
OCStringReplacement >> string: aString [
	string := aString
]
