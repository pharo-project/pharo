"
I represent a Float number
I am stored as two unsigned int32 numbers.
These numbers are got from the instance (Float >> #at:)
When readed back I always create a BoxedFloat. 
Then this instance is operated to generate the correct immediate instance if required.
This hack allows us to load the same hermes file in 32bits or 64 bits
"
Class {
	#name : #HEFloat,
	#superclass : #HEExportedLiteral,
	#category : #'Hermes-Model'
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
