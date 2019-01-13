"
I define a callback for an external function call.
I allow blocks to be evaluated when an external block funtion needs it. 

	cb := FFICallback
			signature:  #(int (const void *a, const void *b))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign].

argument types are calculated and converted using same parsing logic than FFICallout
"
Class {
	#name : #FFICallback,
	#superclass : #Object,
	#instVars : [
		'callback',
		'thunk',
		'functionSpec',
		'block'
	],
	#category : #'UnifiedFFI-Callbacks'
}

{ #category : #converting }
FFICallback class >> asExternalTypeOn: generator [ 
	^ FFICallbackType objectClass: self
]

{ #category : #examples }
FFICallback class >> exampleCqsort [
	<sampleInstance>
	| cb rand values orig sort |

	rand := Random new.
	
	values := FFIExternalArray externalNewType: 'double' size: 100.
	1 to: 100 do: [ :i| values at: i put: rand next ].
	orig := (1 to: 100) collect: [:i| values at: i] as: Array.
	cb := FFICallback
		signature:  #(int (const void *arg1, const void *arg2))
		block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign ].
	[
		self primQsort: values with: 100 with: values type typeSize with: cb.
		sort := values asArray ]
	ensure: [ values free ].
	
	^orig -> sort
]

{ #category : #'instance creation' }
FFICallback class >> forAddress: address [ 
	"Reconstructs a callback from an address (an integer). 
	 This is useful for reading callbacks from a structure. 
	 WARNING: 
		Of course, the callback needs to exist before :)
		And the callback needs to be created with FFICallback (not plain Alien callbacks), then 
		we can be sure block in fact contains the FFICallback instance"
	^ (Callback forAddress: address) evaluator
]

{ #category : #'private primitives' }
FFICallback class >> primQsort: array with: count with: size with: compare [
	self
		ffiCall: #(void qsort (FFIExternalArray array, size_t count, size_t size, FFICallback compare)) 
		module: LibC
]

{ #category : #'instance creation' }
FFICallback class >> signature: aSignature block: aBlock [
	^ self new signature: aSignature block: aBlock
]

{ #category : #evaluation }
FFICallback >> argumentsFor: stackPointer context: callbackContext [

	| callbackArgumentReader | 
	
	callbackArgumentReader := FFIArchitecture forCurrentArchitecture newCallbackArgumentReaderForCallback: self inContext: callbackContext.
	
	functionSpec arguments do: [ :type | callbackArgumentReader extractNext: type ].

	self assert: functionSpec arguments size = callbackArgumentReader arguments size.

	^ callbackArgumentReader arguments.
]

{ #category : #evaluation }
FFICallback >> executeWithArguments: argumentValues [
	
	^ [ block valueWithArguments: argumentValues ]
		on: Error 
		fork: [ :e | e pass ]
		return: [ self returnOnError ]
]

{ #category : #private }
FFICallback >> ffiBindingOf: aName [
	^ self class ffiBindingOf: aName
]

{ #category : #private }
FFICallback >> ffiInstVarArgument: argName generator: generator [
	"Load the instance variable with given name. 
	 It will be ignored anyway in a callback" 
	^ FFIInstVarArgument new 
		argName: argName;
		yourself
]

{ #category : #accessing }
FFICallback >> functionSpec [
	^ functionSpec
]

{ #category : #private }
FFICallback >> newParser [ 
	^ FFIFunctionParser new
		requestor: (FFICallout new
			requestor: self;
			yourself);
		yourself
]

{ #category : #private }
FFICallback >> returnOnError [

	^ functionSpec returnType defaultReturnOnError
]

{ #category : #initialization }
FFICallback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	functionSpec := self newParser parseAnonymousFunction: signature.
	block := aBlock.
	"declare the real callback"
	callback := Callback forCallbackEvaluator: self.
	"keeping the thunk here to easy the FFI method compilation"
	thunk := callback thunk.
]

{ #category : #accessing }
FFICallback >> thunk [
	^ thunk
]

{ #category : #evaluation }
FFICallback >> valueWithContext: callbackContext sp: stackPointer [
	| argumentValues |

	argumentValues := self argumentsFor: stackPointer context: callbackContext.
	^ functionSpec returnType 
		callbackReturnOn: callbackContext 
		for: (self executeWithArguments: argumentValues)
]
