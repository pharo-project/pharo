"
I'm an unsigned byte.
"
Class {
	#name : #FFIUInt8,
	#superclass : #FFIIntegerType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIUInt8 class >> externalType [ 
	^ ExternalType unsignedByte
]

{ #category : #accessing }
FFIUInt8 class >> externalTypeAlignment [
	^ 1
]

{ #category : #accessing }
FFIUInt8 class >> externalTypeSize [
	^ 1
]

{ #category : #private }
FFIUInt8 >> basicHandle: aHandle at: index [
	^ aHandle unsignedByteAt: index
]

{ #category : #private }
FFIUInt8 >> basicHandle: aHandle at: index put: value [
	^ aHandle unsignedByteAt: index put: value
]
