"
I represent a double type in libffi.
I override the marshalling methods from/to C doubles to pharo objects.

See my superclass for more details.
"
Class {
	#name : #TFDoubleType,
	#superclass : #TFBasicType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #writing }
TFDoubleType >> emitMarshallToPrimitive: builder [
	
	builder send: #asFloat
]

{ #category : #reading }
TFDoubleType >> readValue: anExternalAddress offset: offset [
	
	^ anExternalAddress doubleAt: offset
]

{ #category : #writing }
TFDoubleType >> write: aNumber into: anExternalAddress [ 

	anExternalAddress doubleAt: 1 put: aNumber
]
