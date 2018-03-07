"
I represent a boolean.
I am stored in the file as a byte with values 0 (false) and 1 (true).
"
Class {
	#name : #HEBoolean,
	#superclass : #HEExportedLiteral,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEBoolean class >> tag [
	^ 17
]

{ #category : #converting }
HEBoolean >> asLiteralIn: env [
	^ value
]

{ #category : #reading }
HEBoolean >> doReadFrom: aReader [
	value := aReader readByte = 1
]
