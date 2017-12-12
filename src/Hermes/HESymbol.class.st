Class {
	#name : #HESymbol,
	#superclass : #HEByteString,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HESymbol class >> tag [
	^ 9.
]

{ #category : #reading }
HESymbol >> doConvertValue: aValue [
	^ aValue asSymbol.
]
