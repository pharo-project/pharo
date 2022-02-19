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
