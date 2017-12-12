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
