"
I define the type for FFITypeArray objects.
"
Class {
	#name : 'FFITypeArrayType',
	#superclass : 'FFIExternalArrayType',
	#category : 'UnifiedFFI-Types',
	#package : 'UnifiedFFI',
	#tag : 'Types'
}

{ #category : 'private' }
FFITypeArrayType >> annonymousClassCreator [
	^ String streamContents: [ :stream |
		stream
			nextPutAll: '(FFIArray ofType: ';
			print: (self objectClass type isPointer
				ifTrue: [ self externalTypeWithArity printString ]
				ifFalse: [ self objectClass type class name ]);
			nextPutAll: ' size: ';
			print: self objectClass numberOfElements;
			nextPutAll: ')' ]
]

{ #category : 'private' }
FFITypeArrayType >> arrayClassName [
	^ self objectClass isAnonymous
			ifTrue: [ self annonymousClassCreator ]
			ifFalse: [ self objectClass name ]
]

{ #category : 'private' }
FFITypeArrayType >> externalTypeAlignment [
	^ self objectClass type typeAlignment
]

{ #category : 'private' }
FFITypeArrayType >> externalTypeSize [
	^ self objectClass type typeSize * self objectClass numberOfElements
]

{ #category : 'emitting code' }
FFITypeArrayType >> offsetReadArrayAt: offsetVariableName [
	self flag: #pharoTodo. "This is not working in all cases for 32/64 bits (
	externalTypeSize could be different)"
	^ String streamContents:[ :stream|
		stream
			nextPutAll:'^';
			nextPutAll: self arrayClassName;
			nextPutAll:' fromHandle: (handle copyFrom: ';
			nextPutAll: offsetVariableName;
			nextPutAll: ' to: ';
			nextPutAll: (offsetVariableName, ' + ', (self externalTypeSize - 1) asString);
			nextPutAll:')' ]
]

{ #category : 'emitting code' }
FFITypeArrayType >> offsetReadFieldAt: offsetVariableName [
	"Return a string defining the accessor to an entity of the receiver type starting at the given byte offset. "
	self isPointer ifTrue: [ ^ super offsetReadFieldAt: offsetVariableName ].
	^ self offsetReadArrayAt: offsetVariableName
]

{ #category : 'emitting code' }
FFITypeArrayType >> offsetWriteFieldAt: offsetVariableName with: valueName [
	self flag: #pharoTodo. "This is not working in all cases for 32/64 bits (
	externalTypeSize could be different)"
	^ String streamContents:[ :stream|
		stream
			nextPutAll:'handle replaceFrom: ';
			nextPutAll: offsetVariableName;
			nextPutAll: ' to: ';
			nextPutAll: (offsetVariableName, ' + ', (self externalTypeSize - 1) asString);
			nextPutAll: ' with: anObject getHandle startingAt: 1' ]
]

{ #category : 'emitting code' }
FFITypeArrayType >> readArrayAt: byteOffset [
	^ String streamContents:[ :stream|
		stream
			nextPutAll:'^'.
		stream nextPutAll: self arrayClassName.
		stream
			nextPutAll:' fromHandle: (handle copyFrom: ';
			print: byteOffset;
			nextPutAll: ' to: ';
			print: byteOffset + self externalTypeSize - 1;
			nextPutAll:')' ]
]

{ #category : 'emitting code' }
FFITypeArrayType >> readFieldAt: byteOffset [
	"Return a string defining the accessor to an entity of the receiver type starting at the given byte offset. "
	self isPointer ifTrue: [ ^ super readFieldAt: byteOffset ].
	^ self readArrayAt: byteOffset
]

{ #category : 'emitting code' }
FFITypeArrayType >> writeFieldAt: byteOffset with: valueName [
	^ String streamContents:[ :stream|
		stream
			nextPutAll:'handle replaceFrom: ';
			print: byteOffset;
			nextPutAll: ' to: ';
			print: byteOffset + self externalTypeSize - 1;
			nextPutAll: ' with: anObject getHandle startingAt: 1' ]
]
