Class {
	#name : #TFUFFIMarshallingInCallbacksTest,
	#superclass : #TFUFFITestCase,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #testing }
TFUFFIMarshallingInCallbacksTest class >> isAbstract [

	^ self == TFUFFIMarshallingInCallbacksTest
]

{ #category : #tests }
TFUFFIMarshallingInCallbacksTest >> assertCharType: name [

	self assertType: name value: $A
]

{ #category : #tests }
TFUFFIMarshallingInCallbacksTest >> assertSignedIntType: name [

	self assertType: name value: -65
]

{ #category : #tests }
TFUFFIMarshallingInCallbacksTest >> assertType: aType value: aValue [

	| callbackGotValue |
	callbackGotValue := self call: aType value: aValue.
	self assert: callbackGotValue equals: aValue
]

{ #category : #tests }
TFUFFIMarshallingInCallbacksTest >> assertUnsignedIntType: name [

	self assertType: name value: 65
]

{ #category : #tests }
TFUFFIMarshallingInCallbacksTest >> call: aName type: aType value: aValue [

	| callback received |

	callback := self
		newCallbackWithSignature: { #void . aType }
		block: [ :a | received := a ].
	self
		perform: ('simple_callback_f_', aName, ':value:') asSymbol
		withArguments: { callback . aValue}.
	^ received
]

{ #category : #tests }
TFUFFIMarshallingInCallbacksTest >> call: aName value: aValue [

	^ self call: aName type: { aName asSymbol } value: aValue
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_char: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_char(void *callback, char aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_double: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_double(void *callback, double aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_float: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_float(void *callback, float aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_int16: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_int16_t(void *callback, int16 aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_int32: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_int32_t(void *callback, int32 aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_int64: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_int64_t(void *callback, int64 aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_int8: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_int8_t(void *callback, int8 aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_int: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_int(void *callback, int aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_long: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_long(void *callback, long aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_pointer: aCallback value: aValue [

	^ self ffiCall: #(void simple_callback_f_ulonglong(void *aCallback, void* aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_short: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_short(void *callback, short aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_uchar: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_uchar(void *callback, uchar aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_uint16: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_uint16_t(void *callback, uint16 aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_uint32: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_uint32_t(void *callback, uint32 aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_uint64: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_uint64_t(void *callback, uint64 aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_uint8: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_uint8_t(void *callback, uint8 aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_uint: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_uint(void *callback, uint aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_ulong: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_ulong(void *callback, ulong aValue))
]

{ #category : #'ffi-calls' }
TFUFFIMarshallingInCallbacksTest >> simple_callback_f_ushort: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_ushort(void *callback, ushort aValue))
]
