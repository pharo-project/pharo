"
Tests for FFICallout
"
Class {
	#name : #FFICalloutTest,
	#superclass : #TestCase,
	#classVars : [
		'CLASSVAR',
		'TYPEVAR'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #private }
FFICalloutTest >> checkType: type class: typeClass arity: ptrArity argument: arg [ 

	self assert: type resolvedType class equals: typeClass.
	self assert: type resolvedType pointerArity equals: ptrArity.
	self assert: type loader class equals: FFIMethodArgument.
	self assert: type loader argName equals: arg 
]

{ #category : #private }
FFICalloutTest >> checkType: type class: typeClass arity: ptrArity value: anObject [ 

	self assert: type resolvedType class equals: typeClass.
	self assert: type resolvedType pointerArity equals: ptrArity.
	self assert: type loader class equals: FFIConst.
	self assert: type loader value equals: anObject 
]

{ #category : #private }
FFICalloutTest >> checkType: type class: typeClass value: const [
	self assert: type resolvedType class equals: typeClass.
	self assert: type value equals: const
]

{ #category : #private }
FFICalloutTest >> checkTypeSelf: type class: typeClass arity: ptrArity [ 

	self assert: type resolvedType class equals: typeClass.
	self assert: type resolvedType pointerArity equals: ptrArity.
	self assert: type loader class equals: FFISelfArgument.
]

{ #category : #private }
FFICalloutTest >> checkUntypedClassVariable: aFFIConstantArgument class: aTypeClass value: aValue [ 
	
	self assert: aFFIConstantArgument resolvedType class equals: aTypeClass.
	self assert: aFFIConstantArgument resolvedType value equals: aValue.
]

{ #category : #private }
FFICalloutTest >> checkUntypedConstantType: aFFIConstantArgument class: aTypeClass arity: anInteger value: aValue [

	self assert: aFFIConstantArgument resolvedType class equals: aTypeClass.
	self assert: aFFIConstantArgument resolvedType pointerArity equals: anInteger.
	self assert: aFFIConstantArgument loader value equals: aValue.
]

{ #category : #private }
FFICalloutTest >> checkUntypedConstantType: aFFIConstantArgument class: aTypeClass value: aValue [
	
	self checkUntypedConstantType: aFFIConstantArgument class: aTypeClass arity: 0 value: aValue
]

{ #category : #binding }
FFICalloutTest >> ffiBindingOf: aName [
	^ self class ffiBindingOf: aName
]

{ #category : #tests }
FFICalloutTest >> testBeNonStrictTurnsOptionOff [
	|generator|
	generator := FFICallout new.
	
	generator beNonStrict.
	self deny: generator isStrict
]

{ #category : #tests }
FFICalloutTest >> testBeStrictTurnsOptionOn [
	|generator|
	generator := FFICallout new.
	
	generator beStrict.
	self assert: generator isStrict
]

{ #category : #tests }
FFICalloutTest >> testBuildingFnSpec [
	| generator type s |

	generator := FFICallout new.
	
	CLASSVAR := 100.
	TYPEVAR := 'long'.
	
	generator
		requestor: self;
		methodArgs: #(arg1 arg2 arg3 arg11 arg12);
		namedFnSpec: #( long* fn (int self ,  String arg1, void**arg2, TYPEVAR arg1, char arg3, 0, (ulong)42, nil, NULL, true, false , CLASSVAR, ExternalAddress arg11, ByteArray arg12 ) ).

	generator fnSpec resolveUsing: generator.
	type := generator fnSpec returnType.
	self assert: type resolvedType class equals: FFILong.	
	self assert: type resolvedType pointerArity equals: 1.
	
	self assert: (generator fnSpec arguments size) equals: 14.
	s := generator fnSpec arguments readStream.

"int self"		self checkTypeSelf: s next class: FFIInt32 arity: 0.
"String arg1"	self checkType: s next class: FFIExternalString arity: 0 argument: 'arg1'.
"void**arg2"		self checkType: s next class: FFIVoid arity: 2 argument: 'arg2'.
"TYPEVAR arg1"	self checkType: s next class: FFILong arity: 0 argument: 'arg1'.
"char arg3"		self checkType: s next class: FFICharacterType arity: 0 argument: 'arg3'.
"0"					self checkType: s next class: FFIUInt32 value: 0.
"(ulong)42"		self checkType: s next class: FFIULong value: 42.
"nil"				self checkUntypedConstantType: s next class: FFIVoid arity: 1 value: ExternalAddress null.
"NULL"				self checkUntypedConstantType: s next class: FFIVoid arity: 1 value: ExternalAddress null.
"true"				self checkUntypedConstantType: s next class: FFIBool value: 1.
"false"			self checkUntypedConstantType: s next class: FFIBool value: 0.
"CLASSVAR"		self checkUntypedClassVariable: s next class: FFIConst value: 100.
"ExternalAddress"	self checkType: s next class: FFIOop arity: 0 argument: 'arg11'.
"ByteArray" 			self checkType: s next class: FFIOop arity: 0 argument: 'arg12'.
]

{ #category : #tests }
FFICalloutTest >> testParseOptions [
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
	self deny: (generator optionAt: #optEncoding)
	
	
]

{ #category : #tests }
FFICalloutTest >> testStrictModeOptionOff [
	|generator|
	generator := FFICallout new.
	
	generator parseOptions: #(- optStrict ).
	self deny: generator isStrict
]

{ #category : #tests }
FFICalloutTest >> testStrictModeOptionOn [
	|generator|
	generator := FFICallout new.
	
	generator parseOptions: #(+ optStrict ).
	self assert: generator isStrict
]
