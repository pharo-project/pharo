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
	^ aValue.
]

{ #category : #reading }
HEByteString >> doReadFrom: aReader [
	value := self doConvertValue:(aReader readByteString).
]
