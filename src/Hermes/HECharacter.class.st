"
I represent a character.
I serialize myself as a String of a single character.
When I am readed back I just read the first character of the value.
My value is always a String
"
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
