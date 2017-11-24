Class {
	#name : #HEScaledDecimal,
	#superclass : #HEFraction,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEScaledDecimal class >> tag [ 
	^ 25
]

{ #category : #reading }
HEScaledDecimal >> doReadFrom: aReader [
	| scale |
	super doReadFrom: aReader.
	scale := HEExportedLiteral readFrom: aReader.
	
	value := ScaledDecimal newFromNumber: value scale: scale value.
	
	
]

{ #category : #writing }
HEScaledDecimal >> doWriteInto: aWriter [
	super doWriteInto: aWriter.
	value scale asExportedLiteral writeInto: aWriter.	
]
