Class {
	#name : 'TFOptimizedCallsTests',
	#superclass : 'TFTestCase',
	#category : 'ThreadedFFI-Tests',
	#package : 'ThreadedFFI-Tests'
}

{ #category : 'running' }
TFOptimizedCallsTests class >> run [
	
	1000 timesCollect: [ :index | 
		Stdio stdout 
			<< index printString;
			<< ' ';
			<< (self buildSuite run) printString;
			crlf; flush ]
]

{ #category : 'building suites' }
TFOptimizedCallsTests class >> testParameters [

	^ ParametrizedTestMatrix new
		addCase: { #runner -> [ TFSameThreadRunner uniqueInstance ]. #useBytecode -> true};
		yourself


]

{ #category : 'tests' }
TFOptimizedCallsTests >> performTestOn: aBlock argumentTypes: argumentTypes returnType: returnType arguments: arguments validationBlock: validationBlock [

	| callback definition function method |
	
	callback := TFCallback
		            forCallback: aBlock
		            parameters: argumentTypes
		            returnType: returnType
		            runner: runner.

	definition := TFFunctionDefinition
		              parameterTypes: argumentTypes 
		              returnType: returnType.

	function := TFExternalFunction
		            fromAddress: callback getHandle
		            definition: definition.

	function flags: TFExternalFunctionFlags useOptimizedVersion | TFExternalFunctionFlags pointersMightBeExternalAddresses.

	method := self
		          prepareMethodUsingBytecodeFor: function
		          withArguments: arguments.


	10 timesRepeat: [ validationBlock value: method value: arguments ]
]

{ #category : 'tests' }
TFOptimizedCallsTests >> testDoubleToVoid [

	| arg0 |

	self 
		performTestOn: [ :dbl | arg0 := dbl ]
		argumentTypes: {TFBasicType double}
		returnType: TFBasicType void
		arguments: { 17.5 }
		validationBlock: [ :method :args | 			
				arg0 := nil.
				method valueWithReceiver: nil arguments: args.
				self assert: arg0 equals: args first ]
]

{ #category : 'tests' }
TFOptimizedCallsTests >> testPointerDoubleDoubleDoubleDoubleToVoid [

	| arg0 arg1 arg2 arg3 arg4 |

	self 
		performTestOn: [ :ptr :dbl0 :dbl1 :dbl2 :dbl3 | arg0 := ptr. arg1 := dbl0. arg2 := dbl1. arg3 := dbl2. arg4 := dbl3 ]
		argumentTypes: {TFBasicType pointer. TFBasicType double. TFBasicType double. TFBasicType double. TFBasicType double}
		returnType: TFBasicType void
		arguments: { 16r20340 tfPointerAddress. 16.7 . 43.23 . 0.123456789 . -1234.5513e10 }
		validationBlock: [ :method :args | 			
				arg0 := nil.
				arg1 := nil.
				arg2 := nil.
				arg3 := nil.
				method valueWithReceiver: nil arguments: args.
				self assert: arg0 equals: args first.
				self assert: arg1 equals: args second.
				self assert: arg2 equals: args third.
				self assert: arg3 equals: args fourth.
				self assert: arg4 equals: args fifth]
]

{ #category : 'tests' }
TFOptimizedCallsTests >> testPointerDoubleDoubleDoubleToVoid [

	| arg0 arg1 arg2 arg3 |

	self 
		performTestOn: [ :ptr :dbl0 :dbl1 :dbl2 | arg0 := ptr. arg1 := dbl0. arg2 := dbl1. arg3 := dbl2 ]
		argumentTypes: {TFBasicType pointer. TFBasicType double. TFBasicType double. TFBasicType double}
		returnType: TFBasicType void
		arguments: { 16r20340 tfPointerAddress. 16.7 . 43.23 . 0.123456789 }
		validationBlock: [ :method :args | 			
				arg0 := nil.
				arg1 := nil.
				arg2 := nil.
				arg3 := nil.
				method valueWithReceiver: nil arguments: args.
				self assert: arg0 equals: args first.
				self assert: arg1 equals: args second.
				self assert: arg2 equals: args third.
				self assert: arg3 equals: args fourth]
]

{ #category : 'tests' }
TFOptimizedCallsTests >> testPointerDoubleDoubleToVoid [

	| arg0 arg1 arg2 |

	self 
		performTestOn: [ :ptr :dbl0 :dbl1 | arg0 := ptr. arg1 := dbl0. arg2 := dbl1 ]
		argumentTypes: {TFBasicType pointer. TFBasicType double. TFBasicType double}
		returnType: TFBasicType void
		arguments: { 16r20340 tfPointerAddress. 16.7 . 43.23 }
		validationBlock: [ :method :args | 			
				arg0 := nil.
				arg1 := nil.
				arg2 := nil.
				method valueWithReceiver: nil arguments: args.
				self assert: arg0 equals: args first.
				self assert: arg1 equals: args second.
				self assert: arg2 equals: args third]
]

{ #category : 'tests' }
TFOptimizedCallsTests >> testPointerPointerPointerPointerToVoid [

	| arg0 arg1 arg2 arg3 |
	self
		performTestOn: [ :ptr :dbl0 :dbl1 :dbl2 |
			arg0 := ptr.
			arg1 := dbl0.
			arg2 := dbl1.
			arg3 := dbl2 ]
		argumentTypes: {
				TFBasicType pointer.
				TFBasicType pointer.
				TFBasicType pointer.
				TFBasicType pointer }
		returnType: TFBasicType void
		arguments: {
				16r20340 tfPointerAddress.
				16r20341 tfPointerAddress.
				16r20342 tfPointerAddress.
				16r20343 tfPointerAddress }
		validationBlock: [ :method :args |
			arg0 := nil.
			arg1 := nil.
			arg2 := nil.
			arg3 := nil.
			method valueWithReceiver: nil arguments: args.
			self assert: arg0 equals: args first.
			self assert: arg1 equals: args second.
			self assert: arg2 equals: args third.
			self assert: arg3 equals: args fourth ]
]

{ #category : 'tests' }
TFOptimizedCallsTests >> testPointerToPointer [

	| arg0 |

	self 
		performTestOn: [ :ptr | arg0 := ptr. 16r170000 ]
		argumentTypes: {TFBasicType pointer}
		returnType: TFBasicType pointer
		arguments: {16r89495 tfPointerAddress}
		validationBlock: [ :method :args |
			arg0 := nil. 			
			self assert: (method valueWithReceiver: nil arguments: args) equals: 16r170000 tfPointerAddress.
			self assert: arg0 equals: args first ]


]

{ #category : 'tests' }
TFOptimizedCallsTests >> testPointerToVoid [

	| arg0 |

	self 
		performTestOn: [ :ptr | arg0 := ptr ]
		argumentTypes: {TFBasicType pointer}
		returnType: TFBasicType void
		arguments: { 16r20340 tfPointerAddress }
		validationBlock: [ :method :args | 			
				arg0 := nil.
				method valueWithReceiver: nil arguments: args.
				self assert: arg0 equals: args first ]
]

{ #category : 'tests' }
TFOptimizedCallsTests >> testVoidToPointer [

	self 
		performTestOn: [ 16r170000 ]
		argumentTypes: {}
		returnType: TFBasicType pointer
		arguments: {}
		validationBlock: [ :method :args | 			
			self assert: (method valueWithReceiver: nil arguments: args) equals: (ExternalAddress fromAddress: 16r170000) ]


]
