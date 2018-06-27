"
I represent i386 architectures.
"
Class {
	#name : #'FFI_i386',
	#superclass : #FFIArchitecture,
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #testing }
FFI_i386 class >> isActive [
	^ Smalltalk vm wordSize = 4
]

{ #category : #'default abi' }
FFI_i386 >> floatRegisterSize [
	^ 8
]

{ #category : #'default abi' }
FFI_i386 >> integerRegisterSize [
	^ 4
]

{ #category : #'default abi' }
FFI_i386 >> returnSingleFloatsAsDoubles [
	^ true
]
