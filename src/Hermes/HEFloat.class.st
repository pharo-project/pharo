Class {
	#name : #HEFloat,
	#superclass : #HEExportedLiteral,
	#category : 'Hermes-Model'
}

{ #category : #accessing }
HEFloat class >> tag [
	^ 3.
]

{ #category : #converting }
HEFloat >> asLiteralIn: env [
	^ value.
]

{ #category : #reading }
HEFloat >> doReadFrom: aReader [
	| originalValue |
	originalValue := BoxedFloat64 new.
	originalValue at:1 put: (aReader readUInt32).
	originalValue at:2 put: (aReader readUInt32).
	
	"Force the conversion to SmallFloat64 or BoxedFloat64"
	value := originalValue - 0.0.
]

{ #category : #writing }
HEFloat >> doWriteInto: aWriter [
	aWriter writeUInt32: (value at:1).
	aWriter writeUInt32: (value at:2).	
]
