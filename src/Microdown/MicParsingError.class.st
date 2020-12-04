Class {
	#name : #MicParsingError,
	#superclass : #Error,
	#instVars : [
		'line'
	],
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicParsingError >> line [
	^ line
]

{ #category : #accessing }
MicParsingError >> line: anObject [
	line := anObject
]
