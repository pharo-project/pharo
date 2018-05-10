"
I represent a WideString.
I store myself as a byte array.
To store myself I encode the content in utf8.
"
Class {
	#name : #HEWideString,
	#superclass : #HEByteArray,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEWideString class >> tag [
	^ 26
]

{ #category : #reading }
HEWideString >> doReadFrom: aReader [
	value := aReader readByteArray utf8Decoded.
]
