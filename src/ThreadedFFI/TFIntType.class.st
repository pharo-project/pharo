"
I represent a int type in libffi, and I augment basic types with a sign.
I override the marshalling methods from/to C doubles to pharo objects.

See my superclass for more details.
"
Class {
	#name : 'TFIntType',
	#superclass : 'TFBasicType',
	#instVars : [
		'signed'
	],
	#category : 'ThreadedFFI-Types',
	#package : 'ThreadedFFI',
	#tag : 'Types'
}

{ #category : 'marshalling' }
TFIntType >> emitMarshallToPrimitive: builder [

	builder send: #asInteger
]

{ #category : 'marshalling' }
TFIntType >> optimizeFor: aFFIFunctionArgument in: anIRBuilder ifCannot: aFullBlockClosure [

	| literalValue |
	
	aFFIFunctionArgument loader isLiteralArgument 
		ifTrue: [
			literalValue := aFFIFunctionArgument loader value.

			((literalValue isKindOf: Boolean) or: [ (literalValue isKindOf: Number) or: [ literalValue isKindOf: Character ] ])
				ifTrue: [ 
					anIRBuilder pushLiteral: literalValue asInteger.
					^ self ]].

	super
		optimizeFor: aFFIFunctionArgument
		in: anIRBuilder
		ifCannot: aFullBlockClosure
]

{ #category : 'reading' }
TFIntType >> readValue: anExternalAddress offset: offset [

	^ anExternalAddress integerAt: offset size: self byteSize signed: signed
]

{ #category : 'accessing' }
TFIntType >> signed [
	^ signed
]

{ #category : 'accessing' }
TFIntType >> signed: anObject [
	signed := anObject
]

{ #category : 'writing' }
TFIntType >> write: anInteger into: anExternalAddress [

	anExternalAddress integerAt: 1 put: anInteger asInteger size: self byteSize signed: signed
]
