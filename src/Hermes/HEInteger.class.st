Class {
	#name : #HEInteger,
	#superclass : #HEExportedLiteral,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEInteger class >> tag [
	^ 8
]

{ #category : #converting }
HEInteger >> asLiteralIn: env [
	^ value
]

{ #category : #reading }
HEInteger >> doReadFrom: aReader [
	| anArray isNegative |
	isNegative := aReader readByte = 1.
	anArray := aReader readByteArray.

	value := anArray asInteger *(isNegative ifTrue: -1 ifFalse: 1)
]
