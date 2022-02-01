"
I represent a pointer type in libffi.
I override the marshalling methods to allow reading/writing C pointers from/to C pointers.

See my superclass for more details.
"
Class {
	#name : #TFPointerType,
	#superclass : #TFBasicType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #marshalling }
TFPointerType >> emitMarshallToPrimitive: builder [
	
	builder send: #tfPointerAddress
]

{ #category : #marshalling }
TFPointerType >> emitRestorePinnedStateIn: builder of: argument context: sender index: index callout: aCallout [
	
	| tempName |
	tempName := '__pinState', index asString.
	
	argument emitArgument: builder context: sender inCallout: aCallout.
	builder pushTemp: tempName.
	builder send: #setPinnedInMemory:.
	builder popTop.
]

{ #category : #marshalling }
TFPointerType >> emitSavePinnedStateIn: builder index: index [
	
	| tempName |
	tempName := '__pinState', index asString.
	builder addTemp: tempName.
	
	"Finally pin it.
	Pinning returns the previous state.
	Store it to restore it later"
	builder pushDup.
	builder send: #pinInMemory.
	builder storeTemp: tempName.
	builder popTop
]

{ #category : #reading }
TFPointerType >> readValue: anExternalAddress offset: offset [

	^ anExternalAddress pointerAt: offset
]

{ #category : #writing }
TFPointerType >> write: anExternalAddress into: targetAddress [

	targetAddress 
		pointerAt: 1 
		put: anExternalAddress tfPointerAddress
]
