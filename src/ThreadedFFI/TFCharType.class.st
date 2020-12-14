Class {
	#name : #TFCharType,
	#superclass : #TFIntType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #marshalling }
TFCharType >> marshallFromPrimitive: aValue [
	
	^ aValue asCharacter
	
]

{ #category : #marshalling }
TFCharType >> marshallToPrimitive: aValue [
	
	^ aValue asInteger
	
]
