"
I know how to handle the booleans when stored in a int8
"
Class {
	#name : #TFBooleanType,
	#superclass : #TFDerivedType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #accessing }
TFBooleanType >> basicType [

	^ TFBasicType uint8
]

{ #category : #'reading-writing' }
TFBooleanType >> callbackReadValue: anExternalAddress [

	^ (self readValue: anExternalAddress) ~= 0
]

{ #category : #marshalling }
TFBooleanType >> emitMarshallFromPrimitive: anIRBuilder [

	anIRBuilder pushLiteral: 0.
	anIRBuilder send: #~=
]

{ #category : #marshalling }
TFBooleanType >> emitMarshallToPrimitive: builder [

	builder pushLiteral: true.
	builder send: #==.
	builder jumpAheadTo: #falseLabel if: false.
	builder pushLiteral: 1.
	builder jumpAheadTo: #endLabel.
	builder jumpAheadTarget: #falseLabel.
	builder pushLiteral: 0.
	builder jumpAheadTarget: #endLabel
]

{ #category : #'reading-writing' }
TFBooleanType >> readValue: anExternalAddress [
	" Returns 0 or 1, the conversion to false or true is done in the generated method"
	^ self basicType readValue: anExternalAddress
]

{ #category : #'reading-writing' }
TFBooleanType >> write: aBoolean into: aCollection [

	self basicType
		write: aBoolean asBit
		into: aCollection
]
