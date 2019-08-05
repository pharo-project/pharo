"
Tests for external structures on platform
"
Class {
	#name : #FFIExternalStructurePlatformTest,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #'private - testing' }
FFIExternalStructurePlatformTest >> is32bit [
	^ Smalltalk vm is32bit 
		
]

{ #category : #'private - testing' }
FFIExternalStructurePlatformTest >> is64bit [
	^ Smalltalk vm is64bit 
]

{ #category : #tests }
FFIExternalStructurePlatformTest >> testStructureHasCorrectOffsets32bits [
	self is32bit ifFalse: [ ^ self skip ].
	
	FFITestStructureByPlatform compiledSpec. "Ensure fields are initialized"
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_LONG) equals: 1.
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_POINTER) equals: 5
]

{ #category : #tests }
FFIExternalStructurePlatformTest >> testStructureHasCorrectOffsets64bits [
	self is64bit ifFalse: [ ^ self skip ].
	
	FFITestStructureByPlatform compiledSpec. "Ensure fields are initialized"
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_LONG) equals: 1.
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_POINTER) equals: 9
]

{ #category : #tests }
FFIExternalStructurePlatformTest >> testStructureHasCorrectSize32bits [
	self is32bit ifFalse: [ ^ self skip ].
	
	self 
		assert: (FFIExternalType sizeOf: FFITestStructureByPlatform) 
		equals: 8
]

{ #category : #tests }
FFIExternalStructurePlatformTest >> testStructureHasCorrectSize64bits [
	self is64bit ifFalse: [ ^ self skip ].
	
	self 
		assert: (FFIExternalType sizeOf: FFITestStructureByPlatform) 
		equals: 16
]
