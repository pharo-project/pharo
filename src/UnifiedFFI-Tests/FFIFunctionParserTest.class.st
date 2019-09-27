"
Tests for FFIFunctionParser
"
Class {
	#name : #FFIFunctionParserTest,
	#superclass : #FFIAbstractTest,
	#instVars : [
		'ctx',
		'resolver'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #asserting }
FFIFunctionParserTest >> assertIsFalse: anArgument [
	
	self assert: anArgument value equals: 'false'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsNULL: anArgument [
	
	self assert: anArgument value equals: 'NULL'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsNil: anArgument [
	
	self assert: anArgument value equals: 'nil'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsSelf: anArgument [
	
	self assert: anArgument value equals: 'self'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsTrue: anArgument [
	
	self assert: anArgument value equals: 'true'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsVariable: argument named: aName [

	self assert: argument name equals: aName
]

{ #category : #asserting }
FFIFunctionParserTest >> assertType: anArgument named: aTypeName [
	
	self assert: anArgument type name equals: aTypeName
]

{ #category : #asserting }
FFIFunctionParserTest >> assertUndefinedTypeOf: anFFIArgument [
	
	self assert: anFFIArgument type isUndefined
]

{ #category : #asserting }
FFIFunctionParserTest >> assertValue: aLiteralArgument is: aValue [
	
	self assert: aLiteralArgument value equals: aValue
]

{ #category : #helpers }
FFIFunctionParserTest >> ffiInstVarArgument: argName generator: aGenerator [
	
	^ FFIInstVarArgument new 
		argName: argName;
		yourself
]

{ #category : #factory }
FFIFunctionParserTest >> newParser [
	^ FFIFunctionParser new
]

{ #category : #factory }
FFIFunctionParserTest >> newParserWithRequestor [
	
	^ FFIFunctionParser new
		requestor: resolver;
		yourself
]

{ #category : #factory }
FFIFunctionParserTest >> setUp [

	super setUp.
	ctx := Context
		sender: nil
		receiver: FFITestObject new
		method: FFITestObject>>#b:d:
		arguments: #( 25 17 ).
		
	resolver := FFICallout new
		sender: ctx;
		yourself
]

{ #category : #tests }
FFIFunctionParserTest >> testEmptyArgumentReturnsNil [
	| parser |
	
	parser := self newParser
		setStream: 'void' readStream;
		yourself.
	
	self assert: parser parseArgument isNil
]

{ #category : #tests }
FFIFunctionParserTest >> testParseAnonymousFunction [
	 | parser args |
	
	#((int * * ( 0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_))
	  ' int * * (0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_    ' ) 
	do: [:spec |
		parser := self newParser parseAnonymousFunction: spec.
		
		self assert: parser isAnonymous.
		self assert: parser functionName isNil.
		self assert: parser returnType asOldArraySpec equals: #('int' 2).
		
		args := parser arguments.
		self assert: args size equals: 8.

		self assert: (args at: 1) asOldArraySpec equals: #(nil nil 0 0).
		self assert: (args at: 2) asOldArraySpec equals: #(nil nil 'nil' 0).
		self assert: (args at: 3) asOldArraySpec equals: #(nil nil -10 0).
		self assert: (args at: 4) asOldArraySpec equals: #(nil nil 'FOO_BAR' 0). 
		self assert: (args at: 5) asOldArraySpec equals: #('a' nil 'int' 0). 
		self assert: (args at: 6) asOldArraySpec equals: #('_b' nil 'int' 1). 
		self assert: (args at: 7) asOldArraySpec equals: #('c' nil 'char' 2). 
		self assert: (args at: 8) asOldArraySpec equals: #('d_' nil 'void' 3 ) ]
]

{ #category : #tests }
FFIFunctionParserTest >> testParseAnonymousFunctionNoArguments [
	| parser args |
	
	#((int * * ( void ) )
	  ' int * * ( void ) ') 
	do: [:spec |
		parser := self newParser parseAnonymousFunction: spec.
		
		self assert: parser isAnonymous.
		self assert: parser functionName isNil.
		self assert: parser returnType asOldArraySpec equals: #('int' 2).
	
		args := parser arguments.
		self assert: args size equals: 0 ]
]

{ #category : #tests }
FFIFunctionParserTest >> testParseFunction [
	 | parser args |
	
	#((int * * #'function_n$a$m$e' ( 0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_))
	  ' int * * function_n$a$m$e (0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_    ' ) 
	do: [:spec |
		parser := self newParser parseNamedFunction: spec.
		
		self assert: parser functionName equals: 'function_n$a$m$e'.
		self assert: parser returnType asOldArraySpec equals: #('int' 2).
		
		args := parser arguments.
		self assert: args size equals: 8.

		self assert: (args at: 1) asOldArraySpec equals: #(nil nil 0 0). 
		self assert: (args at: 2) asOldArraySpec equals: #(nil nil 'nil' 0). 
		self assert: (args at: 3) asOldArraySpec equals: #(nil nil -10 0). 
		self assert: (args at: 4) asOldArraySpec equals: #(nil nil 'FOO_BAR' 0). 
		self assert: (args at: 5) asOldArraySpec equals: #('a' nil 'int' 0). 
		self assert: (args at: 6) asOldArraySpec equals: #('_b' nil 'int' 1). 
		self assert: (args at: 7) asOldArraySpec equals: #('c' nil 'char' 2). 
		self assert: (args at: 8) asOldArraySpec equals: #('d_' nil 'void' 3 )
	].

]

{ #category : #tests }
FFIFunctionParserTest >> testParseFunction2 [
	#((int * * #'function_n$a$m$e' ( 0, nil, -10," FOO_BAR , "int a, int* _b, char** c, void* * * d_))
	 " ' int * * function_n$a$m$e (0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_ )'" ) 
	do: [:spec | | functionSpec1 functionSpec2 method1 method2 builder |
		functionSpec1 := self newParser parseNamedFunction: spec.
		functionSpec1 resolveUsing: resolver.
		
		functionSpec2 := self newParserWithRequestor parseNamedFunction: spec.
		
		builder := (FFICalloutMethodBuilder calloutAPI: (FFICalloutAPI inContext: nil))
			sender: ctx;
			yourself.
			"signature: functionSignature;
			sender: sender."
		method1 := builder generateMethodFromSpec: functionSpec1.
		method2 := builder generateMethodFromSpec: functionSpec2.
		self assert: method1 bytecode equals: method2 bytecode.
		self assert: method1 literals allButFirst equals: method2 literals allButFirst.
		self assert: (method1 literals first analogousCodeTo: method2 literals first) ].

]

{ #category : #tests }
FFIFunctionParserTest >> testParseFunctionNoArguments [
	| parser args |
	
	#((int * * function_name ( void ) )
	  ' int * * function_name  ( void ) ') 
	do: [:spec |
		parser := self newParser parseNamedFunction: spec.
		
		self assert: parser functionName equals: 'function_name'.
		self assert: parser returnType asOldArraySpec equals: #('int' 2).
	
		args := parser arguments.
		self assert: args size equals: 0 ]
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedVariableArgumentHasName [
	| parser argument |
	
	parser := self newParser
		setStream: 'arbitrary a' readStream;
		yourself.
	
	argument := parser parseArgument.
	self assertIsVariable: argument named: 'a'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedVariableArgumentHasType [
	| parser argument |
	
	parser := self newParser
		setStream: 'arbitrary a' readStream;
		yourself.
	
	argument := parser parseArgument.
	self assertType: argument named: 'arbitrary'
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedConstantIntegerArgumentHasConstantValue [
	| parser argument |
	
	parser := self newParser
		setStream: '1' readStream;
		yourself.

	argument := parser parseArgument.
	self assertValue: argument is: 1
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedConstantIntegerArgumentHasUndefinedType [
	| parser argument |
	
	parser := self newParser
		setStream: '1' readStream;
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedConstantNegativeIntegerArgumentHasConstantValue [
	| parser argument |
	
	parser := self newParser
		setStream: '-1' readStream;
		yourself.

	argument := parser parseArgument.
	self assertValue: argument is: -1
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedConstantNegativeIntegerArgumentHasUndefinedType [
	| parser argument |
	
	parser := self newParser
		setStream: '-1' readStream;
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedFalseArgumentHasUndefinedType [
	| parser argument |
	
	parser := self newParser
		setStream: 'false' readStream;
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedFalseArgumentIsFalse [
	| parser argument |
	
	parser := self newParser
		setStream: 'false' readStream;
		yourself.

	argument := parser parseArgument.
	self assertIsFalse: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedNilArgumentHasUndefinedType [
	| parser argument |
	
	parser := self newParser
		setStream: 'nil' readStream;
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedNilArgumentIsNil [
	| parser argument |
	
	parser := self newParser
		setStream: 'nil' readStream;
		yourself.

	argument := parser parseArgument.
	self assertIsNil: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedNullArgumentHasUndefinedType [
	| parser argument |
	
	parser := self newParser
		setStream: 'NULL' readStream;
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedNullArgumentIsNULL [
	| parser argument |
	
	parser := self newParser
		setStream: 'NULL' readStream;
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedSelfArgumentHasUndefinedType [
	| parser argument |
	
	parser := self newParser
		setStream: 'self' readStream;
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedSelfArgumentIsSelf [
	| parser argument |
	
	parser := self newParser
		setStream: 'self' readStream;
		yourself.

	argument := parser parseArgument.
	self assertIsSelf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedTrueArgumentHasUndefinedType [
	| parser argument |
	
	parser := self newParser
		setStream: 'true' readStream;
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedTrueArgumentIsTrue [
	| parser argument |
	
	parser := self newParser
		setStream: 'true' readStream;
		yourself.

	argument := parser parseArgument.
	self assertIsTrue: argument
]
