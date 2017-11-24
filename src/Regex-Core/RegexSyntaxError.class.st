"
This class represents syntax errors in regular expressions.
"
Class {
	#name : #RegexSyntaxError,
	#superclass : #RegexError,
	#instVars : [
		'position'
	],
	#category : #'Regex-Core-Exceptions'
}

{ #category : #signaling }
RegexSyntaxError class >> signal: anErrorMessage at: errorPosition [
	^ (self new)
		position: errorPosition;
		signal: anErrorMessage
]

{ #category : #accessing }
RegexSyntaxError >> position [
	"return the parsing error location"
	^ position
]

{ #category : #accessing }
RegexSyntaxError >> position: anInteger [
	position := anInteger.
]
