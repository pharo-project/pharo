"
I'm an unsigned int type (32 bits)
"
Class {
	#name : #FFIUInt32,
	#superclass : #FFIIntegerType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIUInt32 class >> externalType [ 
	^ ExternalType ulong
]

{ #category : #accessing }
FFIUInt32 class >> externalTypeAlignment [
	^ 4
]

{ #category : #accessing }
FFIUInt32 class >> externalTypeSize [
	^ 4
]

{ #category : #private }
FFIUInt32 >> basicHandle: aHandle at: index [
	^ aHandle unsignedLongAt: index
]

{ #category : #private }
FFIUInt32 >> basicHandle: aHandle at: index put: value [
	^ aHandle unsignedLongAt: index put: value
]
