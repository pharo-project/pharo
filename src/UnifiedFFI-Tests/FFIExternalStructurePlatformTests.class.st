Class {
	#name : #FFIExternalStructurePlatformTests,
	#superclass : #TestCase,
	#category : #UnifiedFFI-Tests
}

{ #category : #'private - testing' }
FFIExternalStructurePlatformTests >> is32bit [
	^ Smalltalk vm is32bit 
		
]

{ #category : #'private - testing' }
FFIExternalStructurePlatformTests >> is64bit [
	^ Smalltalk vm is64bit 
]

{ #category : #tests }
FFIExternalStructurePlatformTests >> testStructureHasCorrectOffsets32bits [
	self is32bit ifFalse: [ ^ self skip ].
	
	FFITestStructureByPlatform compiledSpec. "Ensure fields are initialized"
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_LONG) equals: 1.
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_POINTER) equals: 5
]

{ #category : #tests }
FFIExternalStructurePlatformTests >> testStructureHasCorrectOffsets64bits [
	self is64bit ifFalse: [ ^ self skip ].
	
	FFITestStructureByPlatform compiledSpec. "Ensure fields are initialized"
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_LONG) equals: 1.
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_POINTER) equals: 9
]

{ #category : #tests }
FFIExternalStructurePlatformTests >> testStructureHasCorrectSize32bits [
	self is32bit ifFalse: [ ^ self skip ].
	
	self 
		assert: (FFIExternalType sizeOf: FFITestStructureByPlatform) 
		equals: 8
]

{ #category : #tests }
FFIExternalStructurePlatformTests >> testStructureHasCorrectSize64bits [
	self is64bit ifFalse: [ ^ self skip ].
	
	self 
		assert: (FFIExternalType sizeOf: FFITestStructureByPlatform) 
		equals: 16
]
