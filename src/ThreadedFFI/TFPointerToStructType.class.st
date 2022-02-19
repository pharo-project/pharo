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
	
	self basicType emitMarshallToPrimitive: anIRBuilder.
]

{ #category : #marshalling }
TFPointerToStructType >> emitRestorePinnedStateIn: builder of: argument context: sender index: index callout: aCallout [
	
	| tempName |
	tempName := '__pinState', index asString.
	
	argument emitArgument: builder context: sender inCallout: aCallout.
	builder pushDup.
	builder send: #isNil.
	builder jumpAheadTo: #notStruct if: true.
	
	"We should pin the handle, not the struct"
	builder send: #getHandle.
	builder pushTemp: tempName.
	builder send: #setPinnedInMemory:.
	
	builder jumpAheadTarget: #notStruct.
	builder popTop.
]

{ #category : #marshalling }
TFPointerToStructType >> emitSavePinnedStateIn: anIRBuilder index: anInteger [
	
	"We should pin the handle, not the struct, only if the handle is not nil"
	anIRBuilder pushDup.
	anIRBuilder send: #isNil.
	anIRBuilder jumpAheadTo: #notStruct if: true.
	
	anIRBuilder send: #getHandle.
	self basicType emitSavePinnedStateIn: anIRBuilder index: anInteger.
	
	anIRBuilder jumpAheadTarget: #notStruct.

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
