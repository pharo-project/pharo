tests
testResolveType
	self assert: (FFIExternalArray resolveType: 'int') class equals: FFIInt32.
	self assert: (FFIExternalArray resolveType: 'String') class equals: FFIExternalString.
	self assert: (FFIExternalArray resolveType: 'char') class equals: FFICharacterType.
	self assert: (FFIExternalArray resolveType: 'void *') class equals: FFIVoid.
	self assert: (FFIExternalArray resolveType: 'void *') pointerArity equals: 1.
	self assert: (FFIExternalArray resolveType: FFIUInt16) class equals: FFIUInt16.
	self assert: (FFIExternalArray resolveType: String) class equals: FFIExternalString.