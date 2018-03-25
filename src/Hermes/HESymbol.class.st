"
I represent the symbols.
I just extend the HEByteString representation.
I just add a conversion from String to Symbol after reading the value.
"
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
