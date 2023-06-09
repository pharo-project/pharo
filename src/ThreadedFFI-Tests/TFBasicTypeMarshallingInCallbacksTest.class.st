Class {
	#name : #TFBasicTypeMarshallingInCallbacksTest,
	#superclass : #TFTestCase,
	#category : #'ThreadedFFI-Tests'
}

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> assertCharType: aType named: name [

	| callbackGotValue |
	callbackGotValue := self call: name type: aType value: $A asInteger.
	self assert: callbackGotValue equals: $A
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> assertIntType: aType named: name value: aValue [

	| callbackGotValue |
	callbackGotValue := self call: name type: aType value: aValue.
	self assert: callbackGotValue equals: aValue
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> assertSignedIntType: aType named: name [

	self assertIntType: aType named: name value: -65
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> assertUnsignedIntType: aType named: name [

	self assertIntType: aType named: name value: 65
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> call: typeName type: type value: aValue [

	| callback fun received |

	callback := TFCallback
		forCallback: [ :a | received := a. ]
		parameters: { type }
		returnType: TFBasicType void
		runner: runner.

	fun := TFExternalFunction
		name: 'simple_callback_f_', typeName
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: { TFBasicType pointer. type }
			returnType: TFBasicType void).

	runner invokeFunction: fun withArguments: { callback getHandle . aValue}.
	^ received
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testChar [

	self assertCharType: TFBasicType schar named: 'char'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testDouble [

	| received |
	received := self call: 'double' type: TFBasicType double value: 2.7.
	self assert: received equals: 2.7
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testFloat [

	| received |
	received := self call: 'float' type: TFBasicType float value: 2.7.
	self assert: (received between: 2.6999 and: 2.7001)
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testPointer [

	| bytes received |
	[
	bytes := ExternalAddress allocate: 2 "bytes".
	received := self call: 'pointer' type: TFBasicType pointer value: bytes.
	self assert: received getHandle equals: bytes getHandle
	] ensure: [ bytes free ]
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testSignedInt [

	self assertSignedIntType: TFBasicType sint named: 'int'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testSignedInt16 [

	self assertSignedIntType: TFBasicType sint16 named: 'int16_t'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testSignedInt32 [

	self assertSignedIntType: TFBasicType sint32 named: 'int32_t'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testSignedInt64 [

	self assertSignedIntType: TFBasicType sint64 named: 'int64_t'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testSignedInt8 [

	self assertSignedIntType: TFBasicType sint8 named: 'int8_t'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testSignedLong [

	self assertSignedIntType: TFBasicType slong named: 'long'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testSignedShort [

	self assertSignedIntType: TFBasicType sshort named: 'short'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testUnsignedInt [

	self assertUnsignedIntType: TFBasicType uint named: 'uint'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testUnsignedInt16 [

	self assertUnsignedIntType: TFBasicType uint16 named: 'uint16_t'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testUnsignedInt32 [

	self assertUnsignedIntType: TFBasicType uint32 named: 'uint32_t'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testUnsignedInt64 [

	self assertUnsignedIntType: TFBasicType uint64 named: 'uint64_t'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testUnsignedInt8 [

	self assertUnsignedIntType: TFBasicType uint8 named: 'uint8_t'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testUnsignedLong [

	self assertUnsignedIntType: TFBasicType ulong named: 'ulong'
]

{ #category : #tests }
TFBasicTypeMarshallingInCallbacksTest >> testUnsignedShort [

	self assertUnsignedIntType: TFBasicType ushort named: 'ushort'
]
