Class {
	#name : #TFUFFICallbackTest,
	#superclass : #TFUFFITestCase,
	#instVars : [
		'oldExceptionHandler'
	],
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #tests }
TFUFFICallbackTest >> callbackFromAnotherThread: callback [

	^ self ffiCall: #(void callbackFromAnotherThread(void* callback))
]

{ #category : #tests }
TFUFFICallbackTest >> callbackInALoop: callback [

	^ self ffiCall: #(int callbackInALoop(void* callback))
]

{ #category : #tests }
TFUFFICallbackTest >> getValue [

	^ self ffiCall: #(int getValue())
]

{ #category : #tests }
TFUFFICallbackTest >> returnAnswer [

	^ self ffiCall: #(int32 returnAnswer())
]

{ #category : #running }
TFUFFICallbackTest >> setUp [ 
	
	super setUp.
	oldExceptionHandler := self ffiLibrary uniqueInstance runner exceptionHandler.
	self ffiLibrary uniqueInstance runner exceptionHandler: TFTestCallbackExceptionHandler new.
]

{ #category : #tests }
TFUFFICallbackTest >> shortCallout [
	
	^ self ffiCall: #(int shortCallout())
]

{ #category : #tests }
TFUFFICallbackTest >> singleCallToCallback: callback value: value [

	^ self ffiCall: #(int singleCallToCallback(void* callback, int value))
]

{ #category : #tests }
TFUFFICallbackTest >> sumFloat: a andDouble: b [

	^ self ffiCall: #(float sumAFloatAndADouble(float a, double b))
]

{ #category : #tests }
TFUFFICallbackTest >> sumInt:a andInt: b [

	^ self ffiCall: #(int sumTwoNumbers(int a, int b))
]

{ #category : #running }
TFUFFICallbackTest >> tearDown [
	
	self ffiLibrary uniqueInstance runner exceptionHandler: oldExceptionHandler.
	super tearDown.

]

{ #category : #tests }
TFUFFICallbackTest >> testCallbackAsFunction [
	| callback fun definition runner |
	
	runner := self ffiLibrary uniqueInstance runner.
	
	callback := self
		newCallbackWithSignature: #(float (int a, float b))
		block: [ :a :b | a + b ].

	definition := TFFunctionDefinition
		parameterTypes: {TFBasicType sint. TFBasicType float.}
		returnType: TFBasicType float.

	fun := TFExternalFunction 
		fromAddress: callback getHandle 
		definition: definition.

	self assert: (runner invokeFunction: fun withArguments: {1. 2.0}) equals: 3.0
]

{ #category : #tests }
TFUFFICallbackTest >> testCallbackInLoop [

	| callback |

	callback := self
		newCallbackWithSignature: #(int (int a))
		block: [ :a | a + 1 ].

	self assert: (self callbackInALoop: callback) equals: 42

]

{ #category : #tests }
TFUFFICallbackTest >> testCallbackInLoopUsingSubclassOfFFICallback [

	| callback |

	callback := TFUFFIIncrementCallback new
		ffiLibrary: self ffiLibrary;
		yourself.

	self assert: (self callbackInALoop: callback) equals: 42

]

{ #category : #tests }
TFUFFICallbackTest >> testCallbackInSingleFunction [

	| callback |

	callback := self newCallbackWithSignature: #(int (int a)) block: [ :a | a + 1 ].
	self assert: (self singleCallToCallback: callback value: 3) equals: 5

]

{ #category : #tests }
TFUFFICallbackTest >> testCallbackInSingleFunctionUsingSubclassOfFFICallback [

	| callback |

	callback := TFUFFIIncrementCallback new
		ffiLibrary: self ffiLibrary;
		yourself.

	self assert: (self singleCallToCallback: callback value: 3) equals: 5

]

{ #category : #tests }
TFUFFICallbackTest >> testCallbackWithErrorHanlded [

	| callback fun definition runner |

	runner := self ffiLibrary uniqueInstance runner.

	callback := TFUFFIFailingCallbackWithDefault new
			ffiLibrary: self ffiLibrary;
			yourself.

	definition := TFFunctionDefinition
		parameterTypes: {TFBasicType sint.}
		returnType: TFBasicType sint.

	fun := TFExternalFunction 
		fromAddress: callback getHandle 
		definition: definition.

	UIManager nonInteractiveDuring: [
		self assert: (runner invokeFunction: fun withArguments: {1}) equals: -1]

]

{ #category : #tests }
TFUFFICallbackTest >> testCallbacksInOtherThread [

	| callback semaphore|

	"Using same thread runner with callbacks from other threads produce a segmentation fault"
	ffiLibrary = TFTestLibraryUsingSameThreadRunner ifTrue: [ ^ self skip ].

	semaphore := Semaphore new.

	callback := self newCallbackWithSignature: #(int (int a)) block: [ :a | semaphore signal. a + 1 ].
	self callbackFromAnotherThread: callback.
	semaphore wait.
	
	1 second wait.
	self assert: self getValue equals: 43	

]
