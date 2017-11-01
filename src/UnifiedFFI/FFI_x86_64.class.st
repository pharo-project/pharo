"
I represent x86_64 architectures.
"
Class {
	#name : #FFI_x86_64,
	#superclass : #FFIArchitecture,
	#category : 'UnifiedFFI'
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
FFI_x86_64 >> externalULongType [
	^ ExternalType unsignedLongLong
]

{ #category : #'default abi' }
FFI_x86_64 >> floatRegisterSize [
	^ 8
]

{ #category : #'default abi' }
FFI_x86_64 >> integerRegisterSize [
	^ 8
]

{ #category : #types }
FFI_x86_64 >> longType [
	^ FFIInt64
]

{ #category : #'default abi' }
FFI_x86_64 >> returnSingleFloatsAsDoubles [
	^ false
]

{ #category : #types }
FFI_x86_64 >> ulongType [
	^ FFIUInt64
]
