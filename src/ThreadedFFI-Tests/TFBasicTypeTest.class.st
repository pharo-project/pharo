Class {
	#name : #TFBasicTypeTest,
	#superclass : #TFTestCase,
	#category : #'ThreadedFFI-Tests'
}

{ #category : #tests }
TFBasicTypeTest >> testBasicTypesAreValid [

	TFBasicType types do: #validate.
	self assert: (TFBasicType types allSatisfy: #isValid)
]

{ #category : #tests }
TFBasicTypeTest >> testCharSize [

	self assert: TFBasicType uchar byteSize equals: 1
]

{ #category : #tests }
TFBasicTypeTest >> testIntSize [

	self assert: TFBasicType uint byteSize equals: 4
]

{ #category : #tests }
TFBasicTypeTest >> testPointerSize [

	self assert: TFBasicType pointer byteSize equals: Smalltalk wordSize
]

{ #category : #tests }
TFBasicTypeTest >> testSInt32Size [

	self assert: TFBasicType sint32 byteSize equals: 4
]

{ #category : #tests }
TFBasicTypeTest >> testSIntSize [

	self assert: TFBasicType sint byteSize equals: 4
]

{ #category : #tests }
TFBasicTypeTest >> testSignedLongUnix [

	OSPlatform current isWindows ifTrue: [
		"Check #testSignedLongWindows"
		^ self skip ].
	self assert: TFBasicType slong byteSize equals: Smalltalk wordSize
]

{ #category : #tests }
TFBasicTypeTest >> testSignedLongWindows [

	OSPlatform current isWindows ifFalse: [
		"Check #testSignedLongUnix"
		^ self skip ].
	self assert: TFBasicType slong byteSize equals: 4
]

{ #category : #tests }
TFBasicTypeTest >> testUInt32Size [

	self assert: TFBasicType uint32 byteSize equals: 4
]

{ #category : #tests }
TFBasicTypeTest >> testUnsignedLongUnix [

	OSPlatform current isWindows ifTrue: [
		"Check #testUnsignedLongWindows"
		^ self skip ].
	self assert: TFBasicType ulong byteSize equals: Smalltalk wordSize
]

{ #category : #tests }
TFBasicTypeTest >> testUnsignedLongWindows [

	OSPlatform current isWindows ifFalse: [
		"Check #testUnsignedLongUnix"
		^ self skip ].
	self assert: TFBasicType ulong byteSize equals: 4
]

{ #category : #tests }
TFBasicTypeTest >> testWriteDouble [

	| aByteArray |

	aByteArray := ByteArray new: 8.
	TFBasicType double write: 1.5 into: aByteArray.

	self assert: (aByteArray doubleAt: 1) equals: 1.5.
	self assert: (TFBasicType double readValue: aByteArray) equals: 1.5
]

{ #category : #tests }
TFBasicTypeTest >> testWriteFloat [

	| aByteArray |

	aByteArray := ByteArray new: 4.
	TFBasicType float write: 1.5 into: aByteArray.

	self assert: (aByteArray floatAt: 1) equals: 1.5.
	self assert: (TFBasicType float readValue: aByteArray) equals: 1.5
]

{ #category : #tests }
TFBasicTypeTest >> testWriteSINT32 [

	| aByteArray |

	aByteArray := ByteArray new: 4.
	TFBasicType sint32 write: -42 into: aByteArray.

	self assert: (aByteArray integerAt: 1 size: 4 signed: true) equals: -42.
	self assert: (TFBasicType sint32 readValue: aByteArray) equals: -42
]

{ #category : #tests }
TFBasicTypeTest >> testWriteUINT32 [

	| aByteArray |

	aByteArray := ByteArray new: 4.
	TFBasicType uint32 write: 42 into: aByteArray.

	self assert: (aByteArray integerAt: 1 size: 4 signed: false) equals: 42.
	self assert: (TFBasicType uint32 readValue: aByteArray) equals: 42
]
