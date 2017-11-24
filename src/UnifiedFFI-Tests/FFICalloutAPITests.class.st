Class {
	#name : #FFICalloutAPITests,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests'
}

{ #category : #running }
FFICalloutAPITests >> ffiCalloutClass [
	^ FFICalloutAPI
]

{ #category : #'primitives atomic' }
FFICalloutAPITests >> ffiCopyString: aString to: dest [
	^ self ffiCall: #( String strcpy ( String dest, String aString ) )
]

{ #category : #'primitives atomic' }
FFICalloutAPITests >> ffiDoubleAbs: number [ 
	^ self ffiCall: #( double DoubleAbsolute (double number) ) options: #(+optCallbackCall)
]

{ #category : #'primitives atomic' }
FFICalloutAPITests >> ffiFloatAbs: number [ 
	^ self ffiCall: #( float FloatAbsolute (float number) ) options: #(+optCallbackCall)
]

{ #category : #'primitives atomic' }
FFICalloutAPITests >> ffiIntAbs: number [
	^ self ffiCall: #( int abs ( int number ) )
]

{ #category : #accessing }
FFICalloutAPITests >> ffiLibraryName [
	^ LibC
]

{ #category : #'primitives atomic' }
FFICalloutAPITests >> ffiLongLongAbs: number [
	^ self ffiCall: #( longlong llabs (longlong number) )
]

{ #category : #'primitives constant' }
FFICalloutAPITests >> ffiTestConstantFormat: format to: buffer [
	^ self ffiCall: #( int sprintf ( ByteArray buffer, String format, 65, 65, true ) )
]

{ #category : #'primitives constant' }
FFICalloutAPITests >> ffiTestContantFormat: format value: aNumber to: buffer [
	^ self ffiCall: #( int sprintf ( ByteArray buffer, String format, 65, 65, long aNumber ) )
]

{ #category : #'primitives atomic' }
FFICalloutAPITests >> ffiToLower: c [
	^ self ffiCall: #( char tolower (char c) )
]

{ #category : #accessing }
FFICalloutAPITests >> method1 [ 
	| object index |
	object := FFICalloutObjectForTest new.
	index := FFICalloutObjectForTest class instVarIndexFor: #handle ifAbsent: [ nil ].
	^ object instVarAt: index.
]

{ #category : #accessing }
FFICalloutAPITests >> method2 [
	| result tmpResult |
	
	result := self call: {1. 2. 3. 4}.
	tmpResult := result.
	result := FFICalloutObjectForTest basicNew.
	result instVarAt: 1 put: tmpResult.	
	^ result 
	
]

{ #category : #'primitives pointer' }
FFICalloutAPITests >> primFromByteArray: src toExternalAddress: dest size: n [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode >
	^ self 
		ffiCall: #( void *memcpy(ExternalAddress dest, ByteArray src, size_t n) )
		module: LibC
]

{ #category : #'primitives pointer' }
FFICalloutAPITests >> primMemMoveFrom: src to: dest size: n [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode >
	^ self 
		ffiCall: #( void *memcpy(void *dest, const void *src, size_t n) )
		module: LibC
]

{ #category : #'primitives pointer' }
FFICalloutAPITests >> primStr: aString cat: otherString [  
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode >
	^ self 
		ffiCall: #( char *strcat ( char *aString, char *otherString ) )
		module: LibC

]

{ #category : #running }
FFICalloutAPITests >> resetFFIMethods [
	| ffiMethods |
	ffiMethods := FFICalloutAPITests methodDict select: [ :method | (method selector beginsWith: 'ffi') or: [method selector beginsWith: 'prim'] ].
	ffiMethods do: #recompile
]

{ #category : #running }
FFICalloutAPITests >> runCase [ 
	| oldAPIClass |
	
	"Ensure I use this class for running the tests"
	oldAPIClass := FFICalloutAPI calloutAPIClass.
	[ FFICalloutAPI calloutAPIClass: FFICalloutAPI. 
	  super runCase ]
	ensure: [ FFICalloutAPI calloutAPIClass: oldAPIClass ]
]

{ #category : #'tests pointer' }
FFICalloutAPITests >> testByteArrayToExternalAddress [
	| result dest |
	

	result := self 
		primFromByteArray: 'Hello, World' asByteArray  
		toExternalAddress: (dest := ExternalAddress allocate: 12) autoRelease 
		size: 12.
		
	self assert: (dest copyFrom: 1 to: 12) asString = 'Hello, World'.
	
]

{ #category : #'tests constant' }
FFICalloutAPITests >> testCallWithAllConstParameter [
	"Tests a function in the form: 
	
	self nbCall: #( long ffiTestInts ( 65, 65, 65, long aNumber ) )
	
	(passing constants as parameters)
	"
	| result buffer |

	buffer := ByteArray new: 100. 
	result := self ffiTestConstantFormat: '%d, %d and %d' to: buffer.
	self assert: result equals: 12.
	self assert: (buffer first: result) asString equals: '65, 65 and 1'
]

{ #category : #'tests constant' }
FFICalloutAPITests >> testCallWithConstParameter [
	"Tests a function in the form: 
	
	self nbCall: #( long ffiTestInts ( 65, 65, 65, long aNumber ) )
	
	(passing constants as parameters)
	"
	| result buffer |

	buffer := ByteArray new: 100. 
	result := self ffiTestContantFormat: '%d, %d and %d' value: true to: buffer.
	self assert: result equals: 12.
	self assert: (buffer first: result) asString equals: '65, 65 and 1'
]

{ #category : #'tests object' }
FFICalloutAPITests >> testCallWithObjectCreation [
	"Test a call of the form: 
	
		self nbCall: #( NBFFICalloutVMObjectForTest malloc ( size_t size ) )
		
		(obtaining an object as result)
	"
	| object |
	
	[
		self shouldnt: [ object := FFICalloutObjectForTest primCreate: 1 ] raise: Error.
		self assert: object notNil.
		self assert: object class = FFICalloutObjectForTest.
		self assert: object handle notNil.
		self deny: object handle isNull ]
	ensure: [ 
		object ifNotNil: [ object free ] ]
]

{ #category : #'tests object' }
FFICalloutAPITests >> testCallWithObjectParameter [
	"Test a call of the form: 
	
		self nbCall: #( void function ( self ) )
		
		(passing self as parameter)
	"
	| object text |
	
	text := 'Hello, World'.
	object := FFICalloutObjectForTest new
		handle: (ByteArray new: text size + 1);
		yourself. 
	
	self 
		shouldnt: [ FFICalloutObjectForTest primObject: object strcpy: text ] 
		raise: Error.
	self assert: object handle asString = (text copyWith: (Character value: 0))
]

{ #category : #'tests object' }
FFICalloutAPITests >> testCallWithSelfParameter [
	"Test a call of the form: 
	
		self nbCall: #( void function ( self ) )
		
		(passing self as parameter)
	"
	| object |
	
	object := FFICalloutObjectForTest new. 
	self 
		shouldnt: [ object strcpy: 'Hello, World' ] 
		raise: Error.
	"Since is a strcpy it will answer a byte array terminated in zero"
	self assert: object handle asString = ('Hello, World' copyWith: (Character value: 0))
]

{ #category : #'tests atomic' }
FFICalloutAPITests >> testCharCall [
	"Test using generic FFI spec"
	| result  |
	
	result := self ffiToLower: $A.
	self assert: result equals: $a.
	 
	result := self ffiToLower: 65.
	self assert: result equals: $a.
	
	result := self ffiToLower: 65.0.
	self assert: result equals: $a.

	result := self ffiToLower: true.
	self assert: result equals: (Character value: 1).

]

{ #category : #'tests pointer' }
FFICalloutAPITests >> testCharPointer [
	| result |
	result := self 
		primStr: ('Hello, ' asByteArray copyWith: 0) 
		cat: ('World' asByteArray copyWith: 0).
	self assert: result = 'Hello, World'.
]

{ #category : #'tests atomic' }
FFICalloutAPITests >> testDoubleCall [
	"Test using generic FFI spec"
	| callback result |
	
	callback := FFICallback 
		signature: #(double (double x)) 
		block: [ :x | x abs ].
	FFICallbackFunctionResolution registerCallback: callback as: #DoubleAbsolute.
	
	result := self ffiDoubleAbs: 65.
	self assert: result equals: 65.0.
	
	result := self ffiDoubleAbs: 65.0.
	self assert: result equals: 65.0.

	result := self ffiDoubleAbs: $A.
	self assert: result equals: 65.0.

	result := self ffiDoubleAbs: true.
	self assert: result equals: 1.0.

]

{ #category : #'tests atomic' }
FFICalloutAPITests >> testFloatCall [
	"Test using generic FFI spec"
	| callback result |
	
	callback := FFICallback 
		signature: #(float (float x)) 
		block: [ :x | x abs ].
	FFICallbackFunctionResolution registerCallback: callback as: #FloatAbsolute.

	result := self ffiFloatAbs: 65.
	self assert: result equals: 65.0.
	
	result := self ffiFloatAbs: 65.0.
	self assert: result equals: 65.0.

	result := self ffiFloatAbs: $A.
	self assert: result equals: 65.0.

	result := self ffiFloatAbs: true.
	self assert: result equals: 1.0.

	

]

{ #category : #'tests atomic' }
FFICalloutAPITests >> testIntCall [
	"Test using generic FFI spec"
	| result |
	
	result := self ffiIntAbs: 65.
	self assert: result equals: 65.
	
	result := self ffiIntAbs: 65.0.
	self assert: result equals: 65.

	result := self ffiIntAbs: $A.
	self assert: result equals: 65.

	result := self ffiIntAbs: true.
	self assert: result equals: 1.

]

{ #category : #'tests atomic' }
FFICalloutAPITests >> testLongLongs [
	"Test passing and returning longlongs"
	| long1 long2 result  |
	
	"This test fails in windows, but it is expected (is because the funtion used is in 
	 another library)"
	Smalltalk os isWindows ifTrue: [ ^ self skip ].		
	
	long1 := 16r123456789012.
	long2 := (-1 << 31).
		
	result := self ffiLongLongAbs: long1.
	self assert: result equals: long1.
	
	result := self ffiLongLongAbs: long2.
	self assert: result equals: long2 abs.
]

{ #category : #'tests atomic' }
FFICalloutAPITests >> testPrintString [
	"Test using generic FFI spec"
	| result buffer |
	
	buffer := ByteArray new: 12.
	result := self ffiCopyString: 'Hello World!' to: buffer.
	self assert: result equals: 'Hello World!'.
	self assert: buffer asString equals: 'Hello World!'.	
]

{ #category : #'tests pointer' }
FFICalloutAPITests >> testVoidPointer [
	| result dest |
	
	result := self 
		primMemMoveFrom: 'Hello, World' asByteArray 
		to: (dest := ByteArray new: 12) 
		size: 12.
		
	self assert: dest asString = 'Hello, World'
]
