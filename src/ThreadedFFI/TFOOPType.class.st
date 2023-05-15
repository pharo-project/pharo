"
I know how to handle the case when we want to send a Pharo object to the FFI.

"
Class {
	#name : #TFOOPType,
	#superclass : #TFDerivedType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #marshalling }
TFOOPType >> emitMarshallFromPrimitive: anIRBuilder [

	anIRBuilder addTemp: #__marshall_temp.
	anIRBuilder storeTemp: #__marshall_temp.
	anIRBuilder popTop.
	anIRBuilder pushLiteral: PointerUtils.
	anIRBuilder pushTemp: #__marshall_temp.
	anIRBuilder send: #objectForOop:
]

{ #category : #marshalling }
TFOOPType >> emitMarshallToPrimitive: anIRBuilder [

	anIRBuilder addTemp: #__marshall_temp.
	anIRBuilder storeTemp: #__marshall_temp.
	anIRBuilder popTop.
	anIRBuilder pushLiteral: PointerUtils.
	anIRBuilder pushTemp: #__marshall_temp.
	anIRBuilder send: #oopForObjectForcePinInMemory:
]

{ #category : #'reading-writing' }
TFOOPType >> readValue: anExternalAddress [
	^ self basicType readValue: anExternalAddress
]
