"
I represent a byteArray. 
The Hermes streams knows how to store byte arrays. 
I only use them.
"
Class {
	#name : #HEByteArray,
	#superclass : #HEExportedLiteral,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEByteArray class >> tag [
	^ 15
]

{ #category : #converting }
HEByteArray >> asLiteralIn: env [
	^ value
]

{ #category : #reading }
HEByteArray >> doReadFrom: aReader [
	value := aReader readByteArray.
]
