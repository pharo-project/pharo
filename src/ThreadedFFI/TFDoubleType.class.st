"
I represent a double type in libffi.
I override the marshalling methods from/to C doubles to pharo objects.

See my superclass for more details.
"
Class {
	#name : 'TFDoubleType',
	#superclass : 'TFBasicType',
	#category : 'ThreadedFFI-Types',
	#package : 'ThreadedFFI',
	#tag : 'Types'
}

{ #category : 'writing' }
TFDoubleType >> emitMarshallToPrimitive: builder [

	builder send: #asFloat
]

{ #category : 'marshalling' }
TFDoubleType >> optimizeFor: aFFIFunctionArgument in: anIRBuilder ifCannot: aFullBlockClosure [

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
TFDoubleType >> readValue: anExternalAddress offset: offset [

	^ anExternalAddress doubleAt: offset
]

{ #category : 'writing' }
TFDoubleType >> write: aNumber into: anExternalAddress [

	anExternalAddress doubleAt: 1 put: aNumber asFloat
]
