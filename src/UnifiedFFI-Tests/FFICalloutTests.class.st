Class {
	#name : #FFICalloutTests,
	#superclass : #TestCase,
	#instVars : [
		'architecture'
	],
	#classVars : [
		'CLASSVAR',
		'TYPEVAR'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #private }
FFICalloutTests >> checkType: type class: typeClass arity: ptrArity argument: arg [ 

	self assert: type class equals: typeClass.
	self assert: type pointerArity equals: ptrArity.
	self assert: type loader class equals: FFIMethodArgument.
	self assert: type loader argName equals: arg 
]

{ #category : #private }
FFICalloutTests >> checkType: type class: typeClass value: const [
	self assert: type class == typeClass.
	self assert: type value = const
]

{ #category : #private }
FFICalloutTests >> checkTypeSelf: type class: typeClass arity: ptrArity [ 

	self assert: type class = typeClass.
	self assert: type pointerArity = ptrArity.
	self assert: type loader class = FFISelfArgument.
]

{ #category : #binding }
FFICalloutTests >> ffiBindingOf: aName [
	^ self class ffiBindingOf: aName
]

{ #category : #running }
FFICalloutTests >> setUp [
	super setUp.
	architecture := FFIArchitecture forCurrentArchitecture
]

{ #category : #tests }
FFICalloutTests >> testBuildingFnSpec [
	| generator type s |

	generator := FFICallout new.
	
	CLASSVAR := 100.
	TYPEVAR := 'long'.
	
	generator
		requestor: self;
		methodArgs: #(arg1 arg2 arg3 arg11 arg12);
		namedFnSpec: #( long* fn (int self ,  String arg1, void**arg2, TYPEVAR arg1, char arg3, 0, nil, true, false , CLASSVAR, ExternalAddress arg11, ByteArray arg12 ) ).


	type := generator fnSpec returnType.
	self assert: type class equals: FFILong.	
	self assert: type pointerArity equals: 1.
	
	self assert: (generator fnSpec arguments size) equals: 12.
	s := generator fnSpec arguments readStream.

"int self"		self checkTypeSelf: s next class: FFIInt32 arity: 0.
"String arg1"	self checkType: s next class: FFIExternalString arity: 0 argument: 'arg1'.
"void**arg2"		self checkType: s next class: FFIVoid arity: 2 argument: 'arg2'.
"TYPEVAR arg1"	self checkType: s next class: FFILong arity: 0 argument: 'arg1'.
"char arg3"		self checkType: s next class: FFICharacterType arity: 0 argument: 'arg3'.
"0"					self checkType: s next class: FFIConst value: 0.
"nil"				self checkType: s next class: FFIConst value: 0.
"true"				self checkType: s next class: FFIConst value: 1.
"false"			self checkType: s next class: FFIConst value: 0.
"CLASSVAR"		self checkType: s next class: FFIConst value: 100.
"ExternalAddress"	self checkType: s next class: FFIOop arity: 0 argument: 'arg11'.
"ByteArray" 			self checkType: s next class: FFIOop arity: 0 argument: 'arg12'.
]

{ #category : #tests }
FFICalloutTests >> testParseOptions [
	|generator|
	generator := FFICallout new.
	
	generator parseOptions: #(optStringOrNil).
	self assert: (generator optionAt: #optStringOrNil).
	generator parseOptions: #(- optStringOrNil).
	self deny: (generator optionAt: #optStringOrNil).
	
	"Lets move on to something a bit more advanced..."
	self deny: (generator optionAt: #optNotThere).
	generator parseOptions: #(+ optEncoding: utf8 optStringOrNil  - optNotThere).
	self assert: (generator optionAt: #optEncoding) equals: #utf8.
	self assert: (generator optionAt: #optStringOrNil).
	
	generator parseOptions: #(- optEncoding).
	self deny: (generator optionAt: #optEncoding).
	
	
]
