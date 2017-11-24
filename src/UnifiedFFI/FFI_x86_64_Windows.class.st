"
I represent the X86_64 architecture, using the Windows X64 ABI.

Parameters passed in integer registers: RCX RDX R8 R9
Parameters passed in floating point registers: XMM0 to XMM3, inclusive
Shadow space for spilling registers: 32 bytes.
Structures are never split into registers with this ABI.

These ABI details are very important to keep in mind for receiving callbacks.
"
Class {
	#name : #'FFI_x86_64_Windows',
	#superclass : #'FFI_x86_64',
	#category : #UnifiedFFI
}

{ #category : #testing }
FFI_x86_64_Windows class >> isActive [
	^ Smalltalk vm wordSize = 8 and: [ OSPlatform current isWindows ]
]

{ #category : #types }
FFI_x86_64_Windows >> externalLongType [
	^ ExternalType long
]

{ #category : #types }
FFI_x86_64_Windows >> externalULongType [
	^ ExternalType ulong
]

{ #category : #'default abi' }
FFI_x86_64_Windows >> floatRegisterCountForParameterPassing [
	^ 4
]

{ #category : #'default abi' }
FFI_x86_64_Windows >> integerRegisterCountForParameterPassing [
	^ 4
]

{ #category : #types }
FFI_x86_64_Windows >> longType [
	^ FFIInt32
]

{ #category : #'default abi' }
FFI_x86_64_Windows >> shadowCallSpaceSize [
	^ 32
]

{ #category : #types }
FFI_x86_64_Windows >> ulongType [
	^ FFIUInt32
]
