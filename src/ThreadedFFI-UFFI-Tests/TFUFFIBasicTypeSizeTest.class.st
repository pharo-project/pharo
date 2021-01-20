Class {
	#name : #TFUFFIBasicTypeSizeTest,
	#superclass : #TFBasicTypeSizeTest,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #accessing }
TFUFFIBasicTypeSizeTest >> assertType: typename equals: aSize [

	self assert: (self perform: ('sizeof_', typename) asSymbol) equals: aSize
]

{ #category : #accessing }
TFUFFIBasicTypeSizeTest >> ffiLibraryName [

	^ self libraryPath
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_char [
	self ffiCall: #(size_t sizeof_char())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_charenum [
	self ffiCall: #(size_t sizeof_charenum())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_double [
	self ffiCall: #(size_t sizeof_double())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_float [
	self ffiCall: #(size_t sizeof_float())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_int [
	self ffiCall: #(size_t sizeof_int())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_int16_t [
	self ffiCall: #(size_t sizeof_int16_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_int32_t [
	self ffiCall: #(size_t sizeof_int32_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_int64_t [
	self ffiCall: #(size_t sizeof_int64_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_int8_t [
	self ffiCall: #(size_t sizeof_int8_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_long [
	self ffiCall: #(size_t sizeof_long())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_longlong [
	self ffiCall: #(size_t sizeof_longlong())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_pointer [
	self ffiCall: #(size_t sizeof_pointer())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_short [
	self ffiCall: #(size_t sizeof_short())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_sintenum [
	self ffiCall: #(size_t sizeof_sintenum())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_size_t [
	self ffiCall: #(size_t sizeof_size_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_uchar [
	self ffiCall: #(size_t sizeof_uchar())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_uint [
	self ffiCall: #(size_t sizeof_uint())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_uint16_t [
	self ffiCall: #(size_t sizeof_uint16_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_uint32_t [
	self ffiCall: #(size_t sizeof_uint32_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_uint64_t [
	self ffiCall: #(size_t sizeof_uint64_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_uint8_t [
	self ffiCall: #(size_t sizeof_uint8_t())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_uintenum [
	self ffiCall: #(size_t sizeof_uintenum())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_ulong [
	self ffiCall: #(size_t sizeof_ulong())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_ulonglong [
	self ffiCall: #(size_t sizeof_ulonglong())
]

{ #category : #'ffi-calls' }
TFUFFIBasicTypeSizeTest >> sizeof_ushort [
	self ffiCall: #(size_t sizeof_ushort())
]
