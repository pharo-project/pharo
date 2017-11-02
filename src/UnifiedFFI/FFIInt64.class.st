"
I'm an signed int type (64bits)
I map to a longlong type. 
"
Class {
	#name : #FFIInt64,
	#superclass : #FFIUInt64,
	#category : 'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIInt64 class >> externalType [ 
	^ ExternalType signedLongLong
]
