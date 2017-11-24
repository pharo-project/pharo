"
I'm an signed byte.
"
Class {
	#name : #FFIInt8,
	#superclass : #FFIUInt8,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIInt8 class >> externalType [ 
	^ ExternalType signedByte
]

{ #category : #private }
FFIInt8 >> basicHandle: aHandle at: index [
	^ aHandle signedByteAt: index
]

{ #category : #private }
FFIInt8 >> basicHandle: aHandle at: index put: value [
	^ aHandle signedByteAt: index put: value
]
