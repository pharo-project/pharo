Class {
	#name : #FFICallbackParametersTests,
	#superclass : #TestCase,
	#instVars : [
		'callback'
	],
	#pools : [
		'FFITestEnumeration'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #executing }
FFICallbackParametersTests >> callCallback: aFFICallback withArgs: aCollection [ 

	^ (ExternalLibraryFunction 
			name: nil
			module: nil
			callType: (OSPlatform current ffiCallingConvention = #stdcall ifTrue: [ 0 ] ifFalse: [ 1 ])
			returnType: aFFICallback functionSpec returnType externalTypeWithArity
			argumentTypes: (aFFICallback functionSpec arguments collect: #externalTypeWithArity))
				setHandle: (ExternalAddress fromAddress: (aFFICallback thunk address)) ;	
					invokeWithArguments: aCollection
	


]

{ #category : #running }
FFICallbackParametersTests >> testCharacterParameters [
	
	| params |
	params := (1 to: 6) collect: [ :i | i asCharacter ].	
	
	callback := FFICallback 
		signature: #(void (char, char, char, char, char, char))
		block: [ :a :b :c :d :e :f  | 
			self assert: a value equals: 1 asCharacter.
			self assert: b value equals: 2 asCharacter.
			self assert: c value equals: 3 asCharacter.
			self assert: d value equals: 4 asCharacter.
			self assert: e value equals: 5 asCharacter.
			self assert: f value equals: 6 asCharacter.
		].

	self callCallback: callback withArgs: params.
]

{ #category : #running }
FFICallbackParametersTests >> testFloatParameters [ 
	
	callback := FFICallback 
		signature: #(void (double a, double b, float c, float d, double e, float f))
		block: [ :a :b :c :d :e :f  | 
			self assert: a value equals: 1.0.
			self assert: b value equals: 2.0.
			self assert: c value equals: 3.0.
			self assert: d value equals: 4.0.
			self assert: e value equals: 5.0.
			self assert: f value equals: 6.0.
		].

	self callCallback: callback withArgs: {1.0. 2.0. 3.0. 4.0. 5.0. 6.0}.
]

{ #category : #running }
FFICallbackParametersTests >> testIntegerParameters [ 
	
	callback := FFICallback 
		signature: #(void (int, int, int, int, int, int))
		block: [ :a :b :c :d :e :f  | 
			self assert: a equals: 1.
			self assert: b equals: 2.
			self assert: c equals: 3.
			self assert: d equals: 4.
			self assert: e equals: 5.
			self assert: f equals: 6.
		].

	self callCallback: callback withArgs: #(1 2 3 4 5 6).


]

{ #category : #running }
FFICallbackParametersTests >> testIntegerPointerParameters [ 
	
	| params |
	params := (1 to: 6) collect: [ :i | ExternalAddress fromAddress: i ].	
	
	callback := FFICallback 
		signature: #(void (void* a, void* b, int* c, int* d, char* e, float* f))
		block: [ :a :b :c :d :e :f  | 
			self assert: a value equals: 1.
			self assert: b value equals: 2.
			self assert: c value equals: 3.
			self assert: d value equals: 4.
			self assert: e value equals: 5.
			self assert: f value equals: 6.
		].

	self callCallback: callback withArgs: params.
]

{ #category : #running }
FFICallbackParametersTests >> testMixingParameters [ 
	
	callback := FFICallback 
		signature: #(void (int a, float b, char c, int d, double e, int f, char g))
		block: [ :a :b :c :d :e :f :g | 
			self assert: a value equals: 1.
			self assert: b value equals: 2.0.
			self assert: c value equals: $c.
			self assert: d value equals: 4.
			self assert: e value equals: 5.0.
			self assert: f value equals: 6.
			self assert: g value equals: $g.
		].

	self callCallback: callback withArgs: {1. 2.0. $c. 4. 5.0. 6. $g}.
]
