"
I represent a pointer type in libffi.
I override the marshalling methods to allow reading/writing C pointers from/to C pointers.

See my superclass for more details.
"
Class {
	#name : 'TFPointerType',
	#superclass : 'TFBasicType',
	#category : 'ThreadedFFI-Types',
	#package : 'ThreadedFFI',
	#tag : 'Types'
}

{ #category : 'marshalling' }
TFPointerType >> emitMarshallToPrimitive: builder [

	builder send: #tfPointerAddress
]

{ #category : 'marshalling' }
TFPointerType >> optimizeFor: aFFIFunctionArgument in: anIRBuilder ifCannot: aFullBlockClosure [

	| literalValue |
	
	aFFIFunctionArgument loader isLiteralArgument 
		ifTrue: [
			literalValue := aFFIFunctionArgument loader value.

			"If I have a SmallInteger literal I can directly to the tfPointerAddress"			
			(literalValue isKindOf: SmallInteger)
				ifTrue: [ 
					anIRBuilder pushLiteral: literalValue tfPointerAddress.
					^ self ].

			"If I have already a Null external address I can avoid the message send"			
			(literalValue = ExternalAddress null) 
				ifTrue: [ anIRBuilder pushLiteral: literalValue. 
					^ self]].

	super
		optimizeFor: aFFIFunctionArgument
		in: anIRBuilder
		ifCannot: aFullBlockClosure
]

{ #category : 'reading' }
TFPointerType >> readValue: anExternalAddress offset: offset [

	^ anExternalAddress pointerAt: offset
]

{ #category : 'writing' }
TFPointerType >> write: anExternalAddress into: targetAddress [

	targetAddress
		pointerAt: 1
		put: anExternalAddress tfPointerAddress
]
