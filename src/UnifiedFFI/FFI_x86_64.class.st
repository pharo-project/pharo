"
I represent x86_64 architectures.
"
Class {
	#name : #'FFI_x86_64',
	#superclass : #FFIArchitecture,
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #testing }
FFI_x86_64 class >> isAbstract [ 

	^self name = #FFI_x86_64
]

{ #category : #types }
FFI_x86_64 >> externalLongType [
	^ ExternalType signedLongLong
]

{ #category : #types }
FFI_x86_64 >> externalSizeTType [
	^ ExternalType unsignedLongLong
]

{ #category : #types }
FFI_x86_64 >> externalULongType [
	^ ExternalType unsignedLongLong
]

{ #category : #'default abi' }
FFI_x86_64 >> returnSingleFloatsAsDoubles [
	^ false
]
