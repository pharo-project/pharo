"
I build FFI method bytecodes for a call. 

"
Class {
	#name : #FFICalloutMethodBuilder,
	#superclass : #Object,
	#instVars : [
		'calloutAPI',
		'requestor',
		'sender',
		'signature',
		'functionResolutionStrategies',
		'library'
	],
	#category : #'UnifiedFFI-Callouts'
}

{ #category : #'instance creation' }
FFICalloutMethodBuilder class >> calloutAPI: aCalloutAPI [ 
	^ self basicNew 
		initializeCalloutAPI: aCalloutAPI;
		yourself
]

{ #category : #accessing }
FFICalloutMethodBuilder >> addFunctionResolveStrategy: aStrategy [
	functionResolutionStrategies add: aStrategy
]

{ #category : #private }
FFICalloutMethodBuilder >> argumentNames [

	^ self method argumentNames
]

{ #category : #building }
FFICalloutMethodBuilder >> build: aBlock [ 
	aBlock value: self.
	^ self generate
]

{ #category : #accessing }
FFICalloutMethodBuilder >> callType [
	self callingConvention = #cdecl ifTrue: [ ^ 0 ].
	self callingConvention = #stdcall ifTrue: [ ^ 1 ].

	self error: 'Invalid call convention!'
]

{ #category : #accessing }
FFICalloutMethodBuilder >> callingConvention [
	^ self calloutAPI callingConvention
]

{ #category : #accessing }
FFICalloutMethodBuilder >> calloutAPI [
	^ calloutAPI
]

{ #category : #'private factory' }
FFICalloutMethodBuilder >> createFFICallout [
	^ FFICallout new
		sender: self sender;
		options: self calloutAPI options;
		yourself
]

{ #category : #'private factory' }
FFICalloutMethodBuilder >> createFFICalloutLiteralFromSpec: functionSpec [
	| externalFunction |
	
	externalFunction := ExternalLibraryFunction
		name: functionSpec functionName
		module: self libraryName
		callType: self callType
		returnType: functionSpec returnType externalTypeWithArity
		argumentTypes: (functionSpec arguments collect: #externalTypeWithArity).
	
	functionResolutionStrategies
		detect: [ :each | each isApplicableFor: self requestor ]
		ifFound: [ :each | each resolve: externalFunction ].
		
	^ externalFunction
]

{ #category : #private }
FFICalloutMethodBuilder >> generate [
	^ self generateMethodFromSpec: (self parseSignature: self signature)
]

{ #category : #private }
FFICalloutMethodBuilder >> generateFFICallout: builder spec: functionSpec [
	| properties |
	"Copy the properties of the old method"
	sender methodProperties
		ifNotNil: [ properties := sender methodProperties copy.
			properties method: nil.
			builder properties: properties ].

	builder
		numArgs: self argumentNames size;
		addTemps: (self argumentNames copyWith: #result).

	"Builds a method call"
	"save ffi call as literal"
	builder pushLiteral: (self createFFICalloutLiteralFromSpec: functionSpec).
	"iterate arguments in order (in the function) to create the function call"
	functionSpec arguments do: [ :each | each emitArgument: builder context: sender inCallout: self requestor ].
	"create the array"
	builder pushConsArray: functionSpec arguments size.
	"send call and store into result"
	builder send: #invokeWithArguments:.
	functionSpec arguments do: [ :each | each emitReturnArgument: builder context: sender ].
	"convert in case return type needs it. And return reseult"
	^ functionSpec returnType
		emitReturn: builder
		resultTempVar: #result
		context: sender
		inCallout: self requestor
]

{ #category : #private }
FFICalloutMethodBuilder >> generateMethodFromSpec: functionSpec [
	| ir |
	functionSpec resolveUsing: self requestor.
	ir := IRBuilder buildIR: [ :builder | 
		self generateFFICallout: builder spec: functionSpec ].
	^ ir generate: self methodTrailer
]

{ #category : #initialization }
FFICalloutMethodBuilder >> initialize [
	super initialize.
	functionResolutionStrategies := FFIFunctionResolutionStrategy allSubclasses collect: #new
		
]

{ #category : #initialization }
FFICalloutMethodBuilder >> initializeCalloutAPI: aCalloutAPI [
	calloutAPI := aCalloutAPI.
	self initialize
]

{ #category : #accessing }
FFICalloutMethodBuilder >> library [
	^ library
]

{ #category : #accessing }
FFICalloutMethodBuilder >> library: aLibrary [
	"A module can be a string with a path to the library or a reference to a class who is child 
	 of FFILibrary.
	 Example: 'libc' or LibC"
	library := aLibrary
]

{ #category : #accessing }
FFICalloutMethodBuilder >> libraryName [
	"Answer the module name, who can come from a string with the path to the module or a 
	 reference to a class who is child of FFILibrary"
	self library ifNil: [ ^ nil ].
	^ self library asFFILibrary libraryName
]

{ #category : #accessing }
FFICalloutMethodBuilder >> method [
	^ self sender method
]

{ #category : #accessing }
FFICalloutMethodBuilder >> methodTrailer [
	
	^ self method trailer
]

{ #category : #private }
FFICalloutMethodBuilder >> parseSignature: aSignature [
	^ FFIFunctionParser new  
		requestor: self requestor;
		parseNamedFunction: aSignature
]

{ #category : #accessing }
FFICalloutMethodBuilder >> requestor [
	^ requestor ifNil: [ requestor := self createFFICallout ]
]

{ #category : #accessing }
FFICalloutMethodBuilder >> requestor: anObject [ 
	"The object who made the request for FFI.
	 By default, NBFFICallout (check #requestor)"
	requestor := anObject
]

{ #category : #accessing }
FFICalloutMethodBuilder >> sender [
	^ sender
]

{ #category : #accessing }
FFICalloutMethodBuilder >> sender: anObject [
	sender := anObject
]

{ #category : #accessing }
FFICalloutMethodBuilder >> signature [
	^ signature
]

{ #category : #accessing }
FFICalloutMethodBuilder >> signature: anObject [
	signature := anObject
]
