Class {
	#name : #HECharacter,
	#superclass : #HEByteString,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HECharacter class >> tag [
	^ 5.
]

{ #category : #converting }
HECharacter >> asLiteralIn: env [
	^ value at:1
]

{ #category : #accessing }
HECharacter >> value: aValue [
	value := aValue asString.
]
