Class {
	#name : 'FFIValueHolderTest',
	#superclass : 'TestCase',
	#category : 'UnifiedFFI-Tests',
	#package : 'UnifiedFFI-Tests'
}

{ #category : 'accessing' }
FFIValueHolderTest >> ffiMethodCharPointerPointer: value [

	^ self 
		ffiCall: #(int TestCharPointerPointerValueHolder(char **value))
		module: 'to-test'
		options: #(+optCallbackCall)
]

{ #category : 'accessing' }
FFIValueHolderTest >> ffiMethodInt: value [

	^ self 
		ffiCall: #(int TestIntValueHolder(int *value))
		module: 'to-test'
		options: #(+optCallbackCall)
]

{ #category : 'accessing' }
FFIValueHolderTest >> ffiMethodIntPointer: value [

	^ self 
		ffiCall: #(int TestIntPointerValueHolder(int *value))
		module: 'to-test'
		options: #(+optCallbackCall)
]

{ #category : 'accessing' }
FFIValueHolderTest >> ffiMethodStructure: value [

	^ self 
		ffiCall: #(int TestStructureValueHolder (FFITestStructure *value))
		module: 'to-test'
		options: #(+optCallbackCall)
]

{ #category : 'running' }
FFIValueHolderTest >> tearDown [

	self class methods
		select: [ :each | (each selector beginsWith: 'ffi') and: [ each hasProperty: #ffiNonCompiledMethod ] ]
		thenDo: [ :each | FFIMethodRegistry uniqueInstance resetMethod: each ].
	FFICallbackFunctionResolution reset.
	
	super tearDown
]

{ #category : 'tests' }
FFIValueHolderTest >> testBasicTypeCanBeHolded [
	| holder |

	holder := FFIInt64 newValueHolder.

	self assert: holder type class equals: FFIInt64.
	self assert: (holder getHandle isKindOf: ByteArray).
	self assert: holder getHandle size equals: FFIInt64 newBuffer size.	
	holder value: 42.
	self assert: holder value equals: 42.
	
	self 
		assert: holder getHandle 
		equals: (FFIInt64 newBuffer
			signedLongAt: 1 put: 42;
			yourself)
]

{ #category : 'tests' }
FFIValueHolderTest >> testCallBasicValueWithValueHolder [
	| holder result resultValue |
	
	holder :=  FFIInt64 newValueHolder.
	
	FFICallbackFunctionResolution 
		registerCallback: (FFICallback 
			signature: #(int (void *aValue)) 
			block: [ :aValue |
				aValue signedLongAt: 1 put: 42.
				1 ])
		as: #TestIntValueHolder.

	result := self ffiMethodInt: holder.
	
	self assert: result equals: 1.
	resultValue := holder value.
	self assert: resultValue equals: 42
]

{ #category : 'tests' }
FFIValueHolderTest >> testCallCharPointerPointerWithValueHolder [
	| holder result string |
	
	holder :=  FFIOop newValueHolder.
	
	FFICallbackFunctionResolution 
		registerCallback: (FFICallback 
			signature: #(int (char **aValue)) 
			block: [ :aValue |
				| externalString |
				externalString := ExternalAddress fromString: 'Hello, World'.
				aValue pointerAt: 1 put: externalString.
				1 ])
		as: #TestCharPointerPointerValueHolder.

	result := self ffiMethodCharPointerPointer: holder.
	
	self assert: result equals: 1.
	string := holder value.
	self 
		assert: string utf8StringFromCString
		equals: 'Hello, World'.
	string free
]

{ #category : 'tests' }
FFIValueHolderTest >> testCallIntPointerWithValueHolder [
	| holder result resultIntegers |
	
	"since I will keep a list of integers (and not just one integer), 
	 I will need a pointer holder (otherwise the buffer will have a wrong size)"
	holder :=  FFIOop newValueHolder.

	FFICallbackFunctionResolution 
		registerCallback: (FFICallback 
			signature: #(int (int *aValue)) 
			block: [ :aValue |
				| integers |
				integers := ExternalAddress allocate: FFIInt32 externalTypeSize * 5.
				integers signedLongAt: 1 put: 1.
				integers signedLongAt: 5 put: 2.
				integers signedLongAt: 9 put: 3.
				integers signedLongAt: 13 put: 4.
				integers signedLongAt: 17 put: 42.
				aValue pointerAt: 1 put: integers.
				1 ])
		as: #TestIntPointerValueHolder.

	result := self ffiMethodIntPointer: holder.

	self assert: result equals: 1.
	resultIntegers := holder arrayOf: #int size: 5.
	self 
		assert: resultIntegers
		equals: #(1 2 3 4 42).
	holder value free
]

{ #category : 'tests' }
FFIValueHolderTest >> testCallStructureWithValueHolder [
	| holder result resultStruct |
	
	holder :=  FFITestStructure newValueHolder.
	
	FFICallbackFunctionResolution 
		registerCallback: (FFICallback 
			signature: #(int (void *aValue)) 
			block: [ :aValue |
				| struct |
				struct := FFITestStructure externalNew.
				struct byte: 42.
				aValue pointerAt: 1 put: struct getHandle.
				1 ])
		as: #TestStructureValueHolder.

	result := self ffiMethodStructure: holder.
	
	self assert: result equals: 1.
	resultStruct := holder value.
	self assert: resultStruct byte equals: 42.
	resultStruct free
]

{ #category : 'tests' }
FFIValueHolderTest >> testOpaqueObjectTypeCanBeHolded [
	| holder |

	holder := FFIOpaqueObject newValueHolder.

	self assert: holder type class equals: FFIOpaqueObject.
	self assert: (holder getHandle isKindOf: ByteArray).
	self assert: holder getHandle size equals: FFIOpaqueObject newBuffer size.	

	holder value: (FFIOpaqueObject fromHandle: (ExternalAddress fromAddress: 42)).
	self assert: holder value getHandle equals: (ExternalAddress fromAddress: 42)
]

{ #category : 'tests' }
FFIValueHolderTest >> testStructureTypeCanBeHolded [
	| holder |
	
	holder :=  FFITestStructure newValueHolder.
	self assert: holder type class equals: FFITestStructure.
	self assert: (holder getHandle isKindOf: ByteArray).
	self assert: holder getHandle size equals: FFITestStructure newBuffer size.	
	"this can't be like this (the structure tries to be filled with the handle)"
	"holder value: (structClass fromHandle: (ExternalAddress fromAddress: 42)).
	self assert: holder value getHandle equals: (ExternalAddress fromAddress: 42)"
]
