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
	#category : #UnifiedFFI
}

{ #category : #converting }
FFICallback class >> asExternalTypeOn: generator [ 
	^ FFICallbackType objectClass: self
]

{ #category : #examples }
FFICallback class >> exampleCqsort [
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

{ #category : #'as yet unclassified' }
FFICallback >> argumentsFor: stackPointer context: callbackContext [
	| index architecture intRegisterCount intRegisterSize intRegisterIndex floatRegisterCount floatRegisterSize floatRegisterIndex floatRegisters intRegisters structureRegisterLayout |
	self flag: 'TODO: Refactor and improve this code.'.
	architecture := FFIArchitecture forCurrentArchitecture.
	
	intRegisterCount := architecture integerRegisterCountForParameterPassing.
	intRegisterSize := architecture integerRegisterSize.
	intRegisterIndex := 0.
	
	floatRegisterCount := architecture floatRegisterCountForParameterPassing.
	floatRegisterSize := architecture floatRegisterSize.
	floatRegisterIndex := 0.
	
	floatRegisters := callbackContext floatregargsp.
	intRegisters := callbackContext intregargsp.
	
	index := 1 + architecture shadowCallSpaceSize.
	^ functionSpec arguments 
		collect: [ :each | | value parameterClass |
			parameterClass := each stackParameterClass.
			(parameterClass == #integer and: [ intRegisterIndex < intRegisterCount and: [each typeSize <= intRegisterSize ]]) ifTrue: [
				value := each callbackValueFor: intRegisters at: intRegisterIndex*intRegisterSize + 1.
				intRegisterIndex := intRegisterIndex + 1
			] ifFalse: [ 
				(parameterClass == #float and: [ floatRegisterIndex < floatRegisterCount and: [each typeSize <= floatRegisterSize ]]) ifTrue: [
					value := each callbackValueFor: floatRegisters at: floatRegisterIndex*floatRegisterSize + 1.
					floatRegisterIndex := floatRegisterIndex + 1
				] ifFalse: [
					(parameterClass == #structure and:
					[ (structureRegisterLayout := architecture computeStructureRegisterPassingLayout: each objectClass) isNotNil and:
					[intRegisterIndex + structureRegisterLayout integerRegisterCount <= intRegisterSize and:
					[floatRegisterIndex + structureRegisterLayout floatRegisterCount <= floatRegisterSize]]]) ifTrue: [
						"This is structure whose content was splitted in registers."
						value := each objectClass new.
						structureRegisterLayout fields do: [ :structureField |
							structureField registerClass == #integer ifTrue: [
								1 to: structureField size do: [ :i |
									value getHandle byteAt: structureField offset + i put:
										(intRegisters unsignedByteAt: intRegisterIndex*intRegisterSize + i)
								].
							] ifFalse: [
								self assert: structureField registerClass == #float.
								1 to: structureField size do: [ :i |
									value getHandle byteAt: structureField offset + i put:
										(floatRegisters unsignedByteAt: floatRegisterIndex*floatRegisterSize + i)
								].
								floatRegisterIndex := floatRegisterIndex + 1
							] 
						]
					] ifFalse: [ 
						"Memory parameter" 
						value := each callbackValueFor: stackPointer at: index.
						index := index + each typeSize.
					]
				].
			].
		
			value ].	
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

{ #category : #private }
FFICallback >> newParser [ 
	^ FFIFunctionParser new
		requestor: (FFICallout new
			requestor: self;
			yourself);
		yourself
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
	
	self flag: #todo. "This can be optimised in a shadow method"
	argumentValues := self argumentsFor: stackPointer context: callbackContext.
	^ functionSpec returnType 
		callbackReturnOn: callbackContext 
		for: (block valueWithArguments: argumentValues)
]
