"
I install callbacks as functions (to be used with ffiCall: later)

I can register any callback with a name, the callback thunk will be used as an entry point for this function.

I implement the option:  optCallbackCall 
	
Example: 
========
callAbsoluteMethod
	 |  callback |

	callback := FFICallback 
		signature: #(double (double x)) 
		block: [ :x | x abs ].
	FFICallbackFunctionResolution register:  callback as:  #absolute.
	self absolute: 42.0.

absolute: aNumber
	self ffiCall: #(double absolute (double aNumber)) options: #(+optCallbackCall)	

"
Class {
	#name : #FFICallbackFunctionResolution,
	#superclass : #FFIFunctionResolutionStrategy,
	#classVars : [
		'CallbackRegistry'
	],
	#category : #'UnifiedFFI-Callouts'
}

{ #category : #private }
FFICallbackFunctionResolution class >> callbackNamed: functionName ifAbsent: aBlock [ 
	^ self callbackRegistry 
		at: functionName asSymbol
		ifAbsent: aBlock
]

{ #category : #private }
FFICallbackFunctionResolution class >> callbackRegistry [ 
	^ CallbackRegistry ifNil: [ CallbackRegistry := Dictionary new ]
]

{ #category : #cleanup }
FFICallbackFunctionResolution class >> cleanUp [
	self reset
]

{ #category : #registration }
FFICallbackFunctionResolution class >> registerCallback: aCallback as: functionName [
	self callbackRegistry 
		at: functionName asSymbol 
		put: aCallback.

]

{ #category : #initialization }
FFICallbackFunctionResolution class >> reset [
	CallbackRegistry := nil
]

{ #category : #registration }
FFICallbackFunctionResolution class >> unregister: functionName [
	self callbackRegistry removeKey: functionName asSymbol 
		
]

{ #category : #testing }
FFICallbackFunctionResolution >> isApplicableFor: aRequestor [
	^ aRequestor optionAt: #optCallbackCall
]

{ #category : #resolving }
FFICallbackFunctionResolution >> resolveFunction: anExternalFunction [
	| callback |
	callback := self class 
		callbackNamed: anExternalFunction name
		ifAbsent: [ self error: 'Callback #', anExternalFunction name, ' not registered!' ].
	^ ExternalAddress fromAddress: callback thunk address
]
