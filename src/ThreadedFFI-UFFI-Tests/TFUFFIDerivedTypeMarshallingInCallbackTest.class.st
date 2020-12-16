Class {
	#name : #TFUFFIDerivedTypeMarshallingInCallbackTest,
	#superclass : #TFUFFIMarshallingInCallbacksTest,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingInCallbackTest >> simple_callback_f_Boolean: callback value: aValue [

	^ self ffiCall: #(void simple_callback_f_int(void *callback, Boolean aValue))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingInCallbackTest >> simple_callback_f_String: callback value: aValue [

	^ self ffiCall: #(void simple_callback_f_pointer(void *callback, String aValue))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingInCallbackTest >> simple_callback_f_longlong: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_longlong(void *callback, longlong aValue))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingInCallbackTest >> simple_callback_f_size_t: callback value: aValue [

	^ self ffiCall: #(void simple_callback_f_size_t(void *callback, size_t aValue))
]

{ #category : #'ffi-calls' }
TFUFFIDerivedTypeMarshallingInCallbackTest >> simple_callback_f_ulonglong: callback value: aValue [
	^ self ffiCall: #(void simple_callback_f_ulonglong(void *callback, ulonglong aValue))
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingInCallbackTest >> testMarshallingFalseArgument [

	self assertType: 'Boolean' value: false
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingInCallbackTest >> testMarshallingSize_t [

	self assertType: 'size_t' value: 17
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingInCallbackTest >> testMarshallingStringArgument [

	self assertType: 'String' value: 'áèïô å∫'
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingInCallbackTest >> testMarshallingTrueArgument [

	self assertType: 'Boolean' value: true
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingInCallbackTest >> testSignedLongLong [

	self assertType: 'longlong' value: -188
]

{ #category : #tests }
TFUFFIDerivedTypeMarshallingInCallbackTest >> testUnsignedLongLong [

	self assertType: 'ulonglong' value: 188
]
