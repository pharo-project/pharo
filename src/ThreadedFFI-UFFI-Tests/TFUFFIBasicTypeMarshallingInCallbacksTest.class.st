Class {
	#name : #TFUFFIBasicTypeMarshallingInCallbacksTest,
	#superclass : #TFUFFIMarshallingInCallbacksTest,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testChar [

	self assertCharType: 'char'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testDouble [

	| received |
	received := self call: 'double' value: 2.7.
	self assert: received equals: 2.7
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testFloat [

	| received |
	received := self call: 'float' value: 2.7.
	self assert: (received between: 2.6999 and: 2.7001)
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testPointer [

	| bytes received |
	[
	bytes := ExternalAddress allocate: 2 "bytes".
	received := self call: 'pointer' type: #(void* name) value: bytes.
	self assert: received getHandle equals: bytes getHandle
	] ensure: [ bytes free ]
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testSignedInt [

	self assertSignedIntType: 'int'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testSignedInt16 [

	self assertSignedIntType: 'int16'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testSignedInt32 [

	self assertSignedIntType: 'int32'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testSignedInt64 [

	self assertSignedIntType: 'int64'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testSignedInt8 [

	self assertSignedIntType: 'int8'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testSignedLong [

	self assertSignedIntType: 'long'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testSignedShort [

	self assertSignedIntType: 'short'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testUnsignedInt [

	self assertUnsignedIntType: 'uint'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testUnsignedInt16 [

	self assertUnsignedIntType: 'uint16'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testUnsignedInt32 [

	self assertUnsignedIntType: 'uint32'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testUnsignedInt64 [

	self assertUnsignedIntType: 'uint64'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testUnsignedInt8 [

	self assertUnsignedIntType: 'uint8'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testUnsignedLong [

	self assertUnsignedIntType: 'ulong'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingInCallbacksTest >> testUnsignedShort [

	self assertUnsignedIntType: 'ushort'
]
