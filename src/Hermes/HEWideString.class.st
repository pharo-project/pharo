Class {
	#name : #HEWideString,
	#superclass : #HEByteArray,
	#category : 'Hermes-Model'
}

{ #category : #accessing }
HEWideString class >> tag [
	^ 26
]

{ #category : #writing }
HEWideString >> doReadFrom: aReader [
	value := aReader readByteArray utf8Decoded.
]

{ #category : #writing }
HEWideString >> doWriteInto: aWriter [
	aWriter writeByteArray: value utf8Encoded.
]
