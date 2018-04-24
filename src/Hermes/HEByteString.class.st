"
I represent a byte string.
Readers and writers knows how to handle byte strings.
I just use them and I provide an entry point to my subclasses to convert the readed value.
"
Class {
	#name : #HEByteString,
	#superclass : #HEExportedLiteral,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEByteString class >> tag [
	^ 4.
]

{ #category : #converting }
HEByteString >> asLiteralIn: env [
	^ value
]

{ #category : #reading }
HEByteString >> doConvertValue: aValue [
	"My subclasses are able to do something else to convert the readed value"
	^ aValue.
]

{ #category : #reading }
HEByteString >> doReadFrom: aReader [
	value := self doConvertValue:(aReader readByteString).
]
