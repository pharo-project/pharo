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
		'backendCallback',
		'functionSpec',
		'block',
		'calloutAPIClass',
		'library'
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
	^ (Callback forAddress: address) evaluator frontendCallback
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

{ #category : #accessing }
FFICallback >> backendCallback [
	"declare the backend callback"
	^ backendCallback ifNil: [ backendCallback := self calloutAPIClass newCallbackBackendFor: self ].

]

{ #category : #accessing }
FFICallback >> beNull [

	self backendCallback beNull
]

{ #category : #accessing }
FFICallback >> calloutAPIClass [

	^ calloutAPIClass ifNil: [ super calloutAPIClass ]
]

{ #category : #accessing }
FFICallback >> calloutAPIClass: anObject [
	calloutAPIClass := anObject
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
FFICallback >> ffiLibrary: aFFILibrary [

	library := aFFILibrary
]

{ #category : #accessing }
FFICallback >> ffiLibraryName [

	^ (library ifNil: [ 'default' ]) asFFILibrary
]

{ #category : #accessing }
FFICallback >> functionSpec [
	^ functionSpec
]

{ #category : #accessing }
FFICallback >> getHandle [
	^ self thunk
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
FFICallback >> signature: signature block: aBlock [
	| parser |
	parser := self newParser.
	functionSpec := parser parseAnonymousFunction: signature.
	functionSpec resolveUsing: parser requestor.
	block := aBlock.

]

{ #category : #accessing }
FFICallback >> thunk [

	(block isNil or: [functionSpec isNil])
		ifTrue: [ ^ ExternalAddress null ].

	^ self backendCallback thunk
]

{ #category : #evaluating }
FFICallback >> valueWithArguments: args [

	^ [ block valueWithArguments: args ]
		on: Error 
		fork: [ :e | e debug ]
		return: [ self returnOnError ]
]
