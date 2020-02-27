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
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #testing }
FFI_x86_64_Windows class >> isActive [
	^ Smalltalk vm wordSize = 8 and: [ OSPlatform current isWindows ]
]

{ #category : #types }
FFI_x86_64_Windows >> bitTypeAlignment [

	^ OSPlatform current ffiLongAlignment
]

{ #category : #types }
FFI_x86_64_Windows >> externalBitType [

	^ ExternalType long
]

{ #category : #types }
FFI_x86_64_Windows >> externalLongType [
	^ ExternalType long
]

{ #category : #types }
FFI_x86_64_Windows >> externalUBitType [

	^ ExternalType ulong
]

{ #category : #types }
FFI_x86_64_Windows >> externalULongType [
	^ ExternalType ulong
]

{ #category : #'instance creation' }
FFI_x86_64_Windows >> newCallbackArgumentReaderForCallback: aCallback inContext: aCallbackContext [ 

	^ FFIWin64CallbackArgumentReader 
		forCallback: aCallback
		inContext: aCallbackContext
]
