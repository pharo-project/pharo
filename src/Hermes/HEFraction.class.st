Class {
	#name : #HEFraction,
	#superclass : #HEExportedLiteral,
	#category : #'Hermes-Model'
}

{ #category : #'as yet unclassified' }
HEFraction class >> tag [ 
	^ 24
]

{ #category : #converting }
HEFraction >> asLiteralIn: aFraction [
	^ value
]

{ #category : #reading }
HEFraction >> doReadFrom: aReader [
	| denominator numerator |
	numerator := HEExportedLiteral readFrom: aReader.
	denominator := HEExportedLiteral readFrom: aReader.
	
	value := Fraction numerator: numerator value denominator: denominator value.
]
