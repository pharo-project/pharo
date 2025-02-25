"
I represent a float type in libffi.
I override the marshalling methods from/to C floats to pharo objects.

See my superclass for more details.
"
Class {
	#name : 'TFFloatType',
	#superclass : 'TFBasicType',
	#category : 'ThreadedFFI-Types',
	#package : 'ThreadedFFI',
	#tag : 'Types'
}

{ #category : 'reading' }
TFFloatType >> emitMarshallToPrimitive: builder [

	builder send: #asFloat
]

{ #category : 'marshalling' }
TFFloatType >> optimizeFor: aFFIFunctionArgument in: anIRBuilder ifCannot: aFullBlockClosure [

	| literalValue |
	
	aFFIFunctionArgument loader isLiteralArgument 
		ifTrue: [
			literalValue := aFFIFunctionArgument loader value.

			(literalValue isKindOf: Number)
				ifTrue: [ 
					anIRBuilder pushLiteral: literalValue asFloat.
					^ self ]].

	super
		optimizeFor: aFFIFunctionArgument
		in: anIRBuilder
		ifCannot: aFullBlockClosure
]

{ #category : 'reading' }
TFFloatType >> readValue: anExternalAddress offset: offset [

	^ anExternalAddress floatAt: offset
]

{ #category : 'writing' }
TFFloatType >> write: aNumber into: anExternalAddress [

	anExternalAddress floatAt: 1 put: aNumber asFloat
]
