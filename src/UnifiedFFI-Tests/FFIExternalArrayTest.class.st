"
Tests for FFIExternalArray
"
Class {
	#name : #FFIExternalArrayTest,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #tests }
FFIExternalArrayTest >> testAt [
	| externalArray |
	
	externalArray := FFIExternalArray newType: 'int' size: 10.
	self assert: externalArray size equals: 10.
	1 to: 10 do: [ :index | self assert: (externalArray at: index) equals: 0 ]

]

{ #category : #tests }
FFIExternalArrayTest >> testAtPut [
	| externalArray |
	
	externalArray := FFIExternalArray newType: 'int' size: 1.
	externalArray at: 1 put: 42.
	self assert: (externalArray at: 1) equals: 42
]

{ #category : #tests }
FFIExternalArrayTest >> testExternalAt [
	| externalArray |
	
	[
	 	externalArray := FFIExternalArray externalNewType: 'int' size: 10.
		self assert: externalArray size equals: 10.
		"External is not initialized, so I cannot check for contents unless I assign them"
		1 to: 10 do: [ :index | externalArray at: index put: 0 ].
		"Testing accessing"
		1 to: 10 do: [ :index | self assert: (externalArray at: index) equals: 0 ].
	] ensure: [ 
		externalArray free ] 

]

{ #category : #tests }
FFIExternalArrayTest >> testExternalAtPut [
	| externalArray |
	
	[	
		externalArray := FFIExternalArray externalNewType: 'int' size: 1.
		externalArray at: 1 put: 42.
		self assert: (externalArray at: 1) equals: 42 ]
	ensure: [ 
		externalArray free ]
]

{ #category : #tests }
FFIExternalArrayTest >> testExternalAtPutFloat [
	| random array |
	random := Random new.
	[
		array := FFIExternalArray externalNewType: 'float' size: 10.
		1 to: 10 do: [ :index | array at: index put: random next ].
		1 to: 10 do: [ :index | self assert: (array at: index) isFloat ] ]
	ensure: [ array free ]
]

{ #category : #tests }
FFIExternalArrayTest >> testResolveType [
	self assert: (FFIExternalArray resolveType: 'int') class equals: FFIInt32.
	self assert: (FFIExternalArray resolveType: 'String') class equals: FFIExternalString.
	self assert: (FFIExternalArray resolveType: 'char') class equals: FFICharacterType.
	self assert: (FFIExternalArray resolveType: 'void *') class equals: FFIVoid.
	self assert: (FFIExternalArray resolveType: 'void *') pointerArity equals: 1.
	self assert: (FFIExternalArray resolveType: FFIUInt16) class equals: FFIUInt16.
	self assert: (FFIExternalArray resolveType: String) class equals: FFIExternalString
]
