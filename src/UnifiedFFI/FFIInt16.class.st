"
I'm an signed short type.
"
Class {
	#name : #FFIInt16,
	#superclass : #FFIUInt16,
	#category : 'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIInt16 class >> externalType [ 
	^ ExternalType short
]

{ #category : #private }
FFIInt16 >> basicHandle: aHandle at: index [
	^ aHandle signedShortAt: index
]

{ #category : #private }
FFIInt16 >> basicHandle: aHandle at: index put: value [ 
	^ aHandle signedShortAt: index put: value
]
