"
I extend TFIntType to support Characters.
"
Class {
	#name : 'TFCharType',
	#superclass : 'TFIntType',
	#category : 'ThreadedFFI-Types',
	#package : 'ThreadedFFI',
	#tag : 'Types'
}

{ #category : 'marshalling' }
TFCharType >> emitMarshallFromPrimitive: builder [

	builder send: #asCharacter
]

{ #category : 'marshalling' }
TFCharType >> marshallFromPrimitive: aValue [

	^ aValue asCharacter
]

{ #category : 'marshalling' }
TFCharType >> marshallToPrimitive: aValue [

	^ aValue asInteger
]

{ #category : 'marshalling' }
TFCharType >> optimizeFor: aFFIFunctionArgument in: anIRBuilder ifCannot: aFullBlockClosure [

	| literalValue |
	
	aFFIFunctionArgument loader isLiteralArgument 
		ifTrue: [
			literalValue := aFFIFunctionArgument loader value.

			((literalValue isKindOf: Character) or: [ literalValue isKindOf: Integer ])
				ifTrue: [ 
					anIRBuilder pushLiteral: literalValue asCharacter.
					^ self ]].

	super
		optimizeFor: aFFIFunctionArgument
		in: anIRBuilder
		ifCannot: aFullBlockClosure
]
