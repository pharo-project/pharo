"
I am a pointer to an Struct. I know how to reconstruct the struct from the received pointer and how to extract the pointer.
The pointer maybe is in the heap of Pharo (it is a ByteArray)
"
Class {
	#name : #TFPointerToStructType,
	#superclass : #TFDerivedType,
	#instVars : [
		'targetClass'
	],
	#category : #'ThreadedFFI-Types'
}

{ #category : #'reading-writing' }
TFPointerToStructType >> callbackReadValue: anExternalAddress [

	^ self basicType callbackReadValue: anExternalAddress
]

{ #category : #marshalling }
TFPointerToStructType >> emitMarshallFromPrimitive: anIRBuilder [

	anIRBuilder addTemp: #__structHandler.
	anIRBuilder storeTemp: #__structHandler.
	anIRBuilder popTop.
	anIRBuilder pushLiteral: targetClass.
	anIRBuilder pushTemp: #__structHandler.
	anIRBuilder send: #fromHandle:
]

{ #category : #marshalling }
TFPointerToStructType >> emitMarshallToPrimitive: anIRBuilder [

	self basicType emitMarshallToPrimitive: anIRBuilder
]

{ #category : #'reading-writing' }
TFPointerToStructType >> readValue: anExternalAddress [

	^ self basicType readValue: anExternalAddress
]

{ #category : #accessing }
TFPointerToStructType >> targetClass [
	^ targetClass
]

{ #category : #accessing }
TFPointerToStructType >> targetClass: anObject [
	targetClass := anObject
]
