"
I extend TFIntType to support Characters.
"
Class {
	#name : #TFCharType,
	#superclass : #TFIntType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #marshalling }
TFCharType >> emitMarshallFromPrimitive: builder [

	builder send: #asCharacter
]

{ #category : #marshalling }
TFCharType >> marshallFromPrimitive: aValue [

	^ aValue asCharacter
]

{ #category : #marshalling }
TFCharType >> marshallToPrimitive: aValue [

	^ aValue asInteger
]
