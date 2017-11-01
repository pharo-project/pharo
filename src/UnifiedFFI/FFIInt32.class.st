"
I'm a signed int type (32 bits)
"
Class {
	#name : #FFIInt32,
	#superclass : #FFIUInt32,
	#category : 'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIInt32 class >> externalType [ 
	^ ExternalType long
]

{ #category : #private }
FFIInt32 >> basicHandle: aHandle at: index [
	^ aHandle signedLongAt: index
]

{ #category : #private }
FFIInt32 >> basicHandle: aHandle at: index put: value [
	^ aHandle signedLongAt: index put: value
]
