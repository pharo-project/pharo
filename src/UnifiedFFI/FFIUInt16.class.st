"
I'm an unsigned short type.
"
Class {
	#name : #FFIUInt16,
	#superclass : #FFIIntegerType,
	#category : #UnifiedFFI-Types
}

{ #category : #accessing }
FFIUInt16 class >> externalType [ 
	^ ExternalType ushort	
]

{ #category : #accessing }
FFIUInt16 class >> externalTypeAlignment [
	^ 2
]

{ #category : #accessing }
FFIUInt16 class >> externalTypeSize [
	^ 2
]

{ #category : #private }
FFIUInt16 >> basicHandle: aHandle at: index [
	^ aHandle unsignedShortAt: index
]

{ #category : #private }
FFIUInt16 >> basicHandle: aHandle at: index put: value [
	^ aHandle unsignedShortAt: index put: value
]
