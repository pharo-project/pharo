Class {
	#name : #TFUFFIBasicTypeMarshallingTest,
	#superclass : #TFUFFITestCase,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #accessing }
TFUFFIBasicTypeMarshallingTest >> assertSignedIntsWithFunction: functionName [

	| return |
	return := self perform: (functionName , ':with:') asSymbol withArguments: { -3 . 2 }.
	self assert: return equals: -1
]

{ #category : #accessing }
TFUFFIBasicTypeMarshallingTest >> assertUnsignedIntsWithFunction: functionName [

	| return |
	return := self perform: (functionName , ':with:') asSymbol withArguments: { 3 . 2 }.
	self assert: return equals: 5
]

{ #category : #accessing }
TFUFFIBasicTypeMarshallingTest >> externalFunction: name ofType: type [

	^ TFExternalFunction
		name: name
		moduleName: self ffiLibrary libraryName
		definition: (TFFunctionDefinition
			parameterTypes: {type. type}
			returnType: type).

]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_char:a with: b [
	^ self ffiCall: #(char sum_char(char a, char b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_double:a with: b [
	^ self ffiCall: #(double sum_double(double a, double b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_float:a with: b [
	^ self ffiCall: #(float sum_float(float a, float b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_int16_t:a with: b [
	^ self ffiCall: #(int16 sum_int16_t(int16 a, int16 b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_int32_t:a with: b [
	^ self ffiCall: #(int32 sum_int32_t(int32 a, int32 b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_int64_t:a with: b [
	^ self ffiCall: #(int64 sum_int64_t(int64 a, int64 b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_int8_t:a with: b [
	^ self ffiCall: #(int8 sum_int8_t(int8 a, int8 b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_int:a with: b [
	^ self ffiCall: #(int sum_int(int a, int b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_long:a with: b [
	^ self ffiCall: #(long sum_long(long a, long b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_short:a with: b [
	^ self ffiCall: #(short sum_short(short a, short b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_uchar:a with: b [
	^ self ffiCall: #(uchar sum_uchar(uchar a, uchar b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_uint16_t:a with: b [
	^ self ffiCall: #(uint16 sum_uint16_t(uint16 a, uint16 b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_uint32_t:a with: b [
	^ self ffiCall: #(uint32 sum_uint32_t(uint32 a, uint32 b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_uint64_t:a with: b [
	^ self ffiCall: #(uint64 sum_uint64_t(uint64 a, uint64 b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_uint8_t:a with: b [
	^ self ffiCall: #(uint8 sum_uint8_t(uint8 a, uint8 b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_uint:a with: b [
	^ self ffiCall: #(uint sum_uint(uint a, uint b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_ulong:a with: b [
	^ self ffiCall: #(ulong sum_ulong(ulong a, ulong b))
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> sum_ushort:a with: b [
	^ self ffiCall: #(ushort sum_ushort(ushort a, ushort b))
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumDouble [

	| return |
	return := self sum_double: 3.1 with: 2.7.
	self assert: (return between: 5.79999999 and: 5.80001)
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumFloat [

	| return |
	return := self sum_float: 3.1 with: 2.7.
	self assert: (return between: 5.79999999 and: 5.80001)
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumSignedChar [

	self assertSignedIntsWithFunction: 'sum_char'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumSignedInt [

	self assertSignedIntsWithFunction: 'sum_int'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumSignedInt16 [

	self assertSignedIntsWithFunction: 'sum_int16_t'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumSignedInt32 [

	self assertSignedIntsWithFunction: 'sum_int32_t'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumSignedInt64 [

	self assertSignedIntsWithFunction: 'sum_int64_t'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumSignedInt8 [

	self assertSignedIntsWithFunction: 'sum_int8_t'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumSignedLong [

	self assertSignedIntsWithFunction: 'sum_long'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumSignedShort [

	self assertSignedIntsWithFunction: 'sum_short'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumUnsignedChar [

	self assertUnsignedIntsWithFunction: 'sum_uchar'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumUnsignedInt [

	self assertUnsignedIntsWithFunction: 'sum_uint'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumUnsignedInt16 [

	self assertUnsignedIntsWithFunction: 'sum_uint16_t'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumUnsignedInt32 [

	self assertUnsignedIntsWithFunction: 'sum_uint32_t'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumUnsignedInt64 [

	self assertUnsignedIntsWithFunction: 'sum_uint64_t'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumUnsignedInt8 [

	self assertUnsignedIntsWithFunction: 'sum_uint8_t'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumUnsignedLong [

	self assertUnsignedIntsWithFunction: 'sum_ulong'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testSumUnsignedShort [

	self assertUnsignedIntsWithFunction: 'sum_ushort'
]

{ #category : #tests }
TFUFFIBasicTypeMarshallingTest >> testUnrefPointer [

	| holder pointerToHolder return |
	holder := ExternalAddress allocate: TFBasicType sshort byteSize.
	TFBasicType sshort write: 17 into: holder.
	
	pointerToHolder := ExternalAddress allocate: TFBasicType pointer byteSize.
	TFBasicType pointer write: holder into: pointerToHolder.
	
	return := self unrefPointer: pointerToHolder.
	self assert: return equals: holder.
	self assert: (TFBasicType sshort readValue: return) equals: 17.
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeMarshallingTest >> unrefPointer: aPointer [
	^ self ffiCall: #(void *unref_pointer(void *aPointer))
]
