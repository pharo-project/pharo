"
I'm an unsigned int type (64bits)
I map to a ulonglong type. 
"
Class {
	#name : #FFIUInt64,
	#superclass : #FFIIntegerType,
	#category : 'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIUInt64 class >> externalType [ 
	^ ExternalType unsignedLongLong
]

{ #category : #accessing }
FFIUInt64 class >> externalTypeAlignment [
	^ OSPlatform current ffiInt64Alignment
]

{ #category : #accessing }
FFIUInt64 class >> externalTypeSize [
	^ 8
]

{ #category : #private }
FFIUInt64 >> basicHandle: aHandle at: index [
	^ aHandle signedLongLongAt: index	

]

{ #category : #private }
FFIUInt64 >> basicHandle: aHandle at: index put: value [ 
	^ aHandle signedLongLongAt: index	put: value

]
