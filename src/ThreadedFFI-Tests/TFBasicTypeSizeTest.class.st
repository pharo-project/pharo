Class {
	#name : #TFBasicTypeSizeTest,
	#superclass : #TFTestCase,
	#category : #'ThreadedFFI-Tests'
}

{ #category : #accessing }
TFBasicTypeSizeTest >> assertType: typename equals: aSize [

	| function |
	function := self externalFunction: 'sizeof_', typename.
	self assert: (runner invokeFunction: function) equals: aSize
]

{ #category : #accessing }
TFBasicTypeSizeTest >> externalFunction: name [

	^ TFExternalFunction
		name: name
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: {}
			returnType: TFBasicType sizeT)

]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeCharEnum [

	self assertType: 'charenum' equals: 4.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeDouble [

	self assertType: 'double' equals: 8.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeFloat [

	self assertType: 'float' equals: 4.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeLongLong [

	self assertType: 'longlong' equals: 8
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizePointer [

	self assertType: 'pointer' equals: Smalltalk wordSize
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedChar [

	self assertType: 'char' equals: 1.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedInt [

	self assertType: 'int' equals: 4.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedInt16 [

	self assertType: 'int16_t' equals: 2
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedInt32 [

	self assertType: 'int32_t' equals: 4
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedInt64 [

	self assertType: 'int64_t' equals: 8
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedInt8 [

	self assertType: 'int8_t' equals: 1
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedIntEnum [

	self assertType: 'sintenum' equals: 4.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedLongLong [

	self assertType: 'longlong' equals: 8
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedLongUnix [

	OSPlatform current isWindows ifTrue: [ 
		"Check #testSizeSignedLongWindows"
		^ self skip ].
	self assertType: 'long' equals: Smalltalk wordSize
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedLongWindows [

	OSPlatform current isWindows ifFalse: [ 
		"Check #testSizeSignedLongUnix"
		^ self skip ].
	self assertType: 'long' equals: 4
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeSignedShort [

	self assertType: 'short' equals: 2
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedChar [

	self assertType: 'uchar' equals: 1.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedInt [

	self assertType: 'uint' equals: 4.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedInt16 [

	self assertType: 'uint16_t' equals: 2
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedInt64 [

	self assertType: 'uint64_t' equals: 8
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedInt8 [

	self assertType: 'uint8_t' equals: 1
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedIntEnum [

	self assertType: 'uintenum' equals: 4.
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedLongLong [

	self assertType: 'ulonglong' equals: 8
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedLongUnix [

	OSPlatform current isWindows ifTrue: [ 
		"Check #testSizeUnsignedLongWindows"
		^ self skip ].
	self assertType: 'ulong' equals: Smalltalk wordSize
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedLongWindows [

	OSPlatform current isWindows ifFalse: [ 
		"Check #testSizeUnsignedLongUnix"
		^ self skip ].
	self assertType: 'ulong' equals: 4
]

{ #category : #tests }
TFBasicTypeSizeTest >> testSizeUnsignedShort [

	self assertType: 'ushort' equals: 2
]
