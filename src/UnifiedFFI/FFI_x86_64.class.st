"
I represent x86_64 architectures.
"
Class {
	#name : #'FFI_x86_64',
	#superclass : #FFIArchitecture,
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #private }
FFI_x86_64 class >> uniqueInstance [
	self = FFI_x86_64 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children instead.' ].
	^ super uniqueInstance
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
