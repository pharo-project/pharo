"
Unit tests for FFI architectures
"
Class {
	#name : #FFIArchitectureTest,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Architecture'
}

{ #category : #tests }
FFIArchitectureTest >> testAbstractArchitectures [

	self assert: FFIArchitecture isAbstract.
	self assert: FFI_x86_64 isAbstract.
]

{ #category : #tests }
FFIArchitectureTest >> testArchitectureProvided [

	self assert: FFIArchitecture forCurrentArchitecture isNotNil 
]

{ #category : #tests }
FFIArchitectureTest >> testConcreteArchitectures [

	self deny: FFI_i386 isAbstract.
	self deny: FFI_x86_64_SystemV isAbstract.
	self deny: FFI_x86_64_Windows isAbstract.
]
