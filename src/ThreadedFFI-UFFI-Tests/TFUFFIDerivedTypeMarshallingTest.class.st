Class {
	#name : #TFUFFIDerivedTypeMarshallingTest,
	#superclass : #TFUFFITestCase,
	#classVars : [
		'gchar'
	],
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> booleanToInt: boolean [

	^ self ffiCall: #(int id_int(Boolean boolean))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> fillByteArray: aByteArray size: size [

	^ self ffiCall: #(void fillByteArray(oop aByteArray, int size))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> intToBoolean: int [

	^ self ffiCall: #(Boolean id_int(int int))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> pointerToString: cString [

	^ self ffiCall: #(String dup_string(char *cString))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> pointerToStringWithAlias: cString [

	^ self ffiCall: #(String dup_string(gchar *cString))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> returnSameByteArray: aByteArray [

	^ self ffiCall: #(oop id_pointer(oop aByteArray))
]

{ #category : #running }
TFUFFIDerivedTypeMarshallingTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	gchar := 'char'
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> size_t_identity: aSize [

	^ self ffiCall: #(size_t id_size_t(size_t aSize))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> stringToPointer: aString [

	^ self ffiCall: #(void *dup_string(String aString))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> sum_longlong:a with: b [

	^ self ffiCall: #(longlong sum_longlong(longlong a, longlong b))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingTest >> sum_ulonglong:a with: b [

	^ self ffiCall: #(ulonglong sum_ulonglong(ulonglong a, ulonglong b))
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingFalseArgument [

	self assert: (self booleanToInt: false) equals: 0
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingFalseReturnValue [

	self assert: (self intToBoolean: 0) equals: false
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingOOP [
	| aByteArray |
	aByteArray := ByteArray new: 10.
	aByteArray pinInMemory.
	
	self fillByteArray: aByteArray size: aByteArray size.
	
	self assert: aByteArray equals: #[1 2 3 4 5 6 7 8 9 10].
	
	aByteArray unpinInMemory.
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingOOPIsSameObject [
	| aByteArray otherByteArray |
	aByteArray := ByteArray new: 2300000.
	aByteArray pinInMemory.
	
	otherByteArray := self returnSameByteArray: aByteArray.
	
	self assert: aByteArray == otherByteArray.
	
	aByteArray unpinInMemory.
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingSize_t [

	self assert: (self size_t_identity: 1788) equals: 1788
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingSmalltalkStringReturnValueWithAlias [

	| originalString returnedString |

	originalString := 'áèïô å∫'.
	returnedString := self pointerToStringWithAlias: originalString.
	self assert: returnedString equals: originalString.

]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingStringArgument [

	| stringToMarshall cString |
	[
	stringToMarshall := 'áèïô å∫'.
	cString := self stringToPointer: stringToMarshall.
	self assert: cString readStringUTF8 equals: stringToMarshall.
	] ensure: [ cString free ]
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingStringReturnValue [

	| cString originalString returnedString |
	[
	originalString := 'áèïô å∫'.
	cString := ExternalAddress fromString: originalString utf8Encoded asString.
	returnedString := self pointerToString: cString.
	self assert: returnedString equals: originalString.
	] ensure: [ cString free ]
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingStringReturnValueWithAlias [

	| cString originalString returnedString |
	[
	originalString := 'áèïô å∫'.
	cString := ExternalAddress fromString: originalString utf8Encoded asString.
	returnedString := self pointerToStringWithAlias: cString.
	self assert: returnedString equals: originalString.
	] ensure: [ cString free ]
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingTrueArgument [

	self assert: (self booleanToInt: true) equals: 1
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testMarshallingTrueReturnValue [

	self assert: (self intToBoolean: 17) equals: true
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testSumSignedLongLong [

	| return |
	return := self sum_longlong: 3 with: 2.
	self assert: return equals: 5
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingTest >> testUnsignedSumLongLong [

	| return |
	return := self sum_longlong: -3 with: 2.
	self assert: return equals: -1
]
