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
	
	self assert: anArgument value equals: false
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsNULL: anArgument [
	
	self assert: anArgument value equals: 'NULL'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsNil: anArgument [
	
	self assert: anArgument value equals: nil
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsSelf: anArgument [
	
	self assert: anArgument value equals: 'self'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsTrue: anArgument [
	
	self assert: anArgument value equals: true
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsVariable: argument named: aName [

	self assert: argument name equals: aName
]

{ #category : #asserting }
FFIFunctionParserTest >> assertType: anArgument arity: anArity [
	
	self assert: anArgument type arity equals: anArity
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
	
	^ self newParser
		requestor: resolver;
		yourself
]

{ #category : #tests }
FFIFunctionParserTest >> parseArgument: inputToParse [
	| parser |
	
	parser := self newParser
		setOn: inputToParse;
		yourself.

	^ parser parseArgument
]

{ #category : #running }
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
FFIFunctionParserTest >> testCastedTypedConstantIntegerHasConstantValue [
	| argument |
	argument := self parseArgument: '#(#ulong) 42'.
	self assertValue: argument is: 42
]

{ #category : #tests }
FFIFunctionParserTest >> testCastedTypedConstantIntegerHasType [
	| argument |
	argument := self parseArgument: '#(#ulong) 42'.
	self assertType: argument named: 'ulong'
]

{ #category : #tests }
FFIFunctionParserTest >> testEmptyArgumentReturnsNil [
	| argument |
	argument := self parseArgument: 'void'.
	self assert: argument isNil
]

{ #category : #tests }
FFIFunctionParserTest >> testParseAnonymousFunction [
	 | parser args |
	
	#((int * * ( 0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_))
	  ' int * * (0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_ )' ) 
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
	  ' int * * #''function_n$a$m$e'' (0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_ )' ) 
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
	#((int * * #'function_n$a$m$e' ( self, 0, nil, -10, true, false, " FOO_BAR , "int a, int* _b, char** c, void* * * d_))
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
FFIFunctionParserTest >> testParseFunctionArrayHasFunctionName [

	| spec |
	spec := self newParser parseNamedFunction: #( type f ( void ) ).
	self assert: spec functionName equals: 'f'
]

{ #category : #tests }
FFIFunctionParserTest >> testParseFunctionArrayWithPointerReturnTypeHasFunctionName [

	| spec |
	spec := self newParser parseNamedFunction: #( type * f ( void ) ).
	self assert: spec functionName equals: 'f'
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
FFIFunctionParserTest >> testTypedConstantIntegerArgumentHasConstantValue [
	| argument |
	argument := self parseArgument: 'int 1'.
	self assertValue: argument is: 1
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedConstantIntegerArgumentHasDefinedType [
	| argument |
	argument := self parseArgument: 'int 1'.
	self assertType: argument named: 'int'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedConstantIntegerArgumentHasZeroArity [
	| argument |
	argument := self parseArgument: 'int 1'.
	self assertType: argument arity: 0
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedDoublePointerVariableArgumentHasArity [
	| argument |
	argument := self parseArgument: 'arbitrary** a'.
	self assertType: argument arity: 2
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedDoublePointerVariableArgumentHasName [
	| argument |
	argument := self parseArgument: 'arbitrary** a'.
	self assertIsVariable: argument named: 'a'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedDoublePointerVariableArgumentHasType [
	| argument |
	argument := self parseArgument: 'arbitrary** a'.
	self assertType: argument named: 'arbitrary'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedFalseArgumentHasDefinedType [
	| argument |
	argument := self parseArgument: 'int64 false'.
	self assertType: argument named: 'int64'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedFalseArgumentIsFalse [
	| argument |
	argument := self parseArgument: 'int false'.
	self assertIsFalse: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedFloatArgumentHasDefinedType [
	| argument |
	argument := self parseArgument: 'int64 1.32'.
	self assertType: argument named: 'int64'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedFloatArgumentIsFloat [
	| argument |
	argument := self parseArgument: 'int 1.32'.
	self assertValue: argument is: 1.32
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedPointerVariableArgumentHasArity [
	| argument |
	argument := self parseArgument: 'arbitrary* a'.
	self assertType: argument arity: 1
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedPointerVariableArgumentHasName [
	| argument |
	argument := self parseArgument: 'arbitrary* a'.
	self assertIsVariable: argument named: 'a'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedPointerVariableArgumentHasType [
	| argument |
	argument := self parseArgument: 'arbitrary* a'.
	self assertType: argument named: 'arbitrary'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedStringArgumentHasDefinedType [
	| argument |
	argument := self parseArgument: 'stringy ''a string'''.
	self assertType: argument named: 'stringy'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedStringArgumentIsString [
	| argument |
	argument := self parseArgument: 'stringy ''a stringy'''.
	self assertValue: argument is: 'a stringy'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedStringSelfArgumentHasDefinedType [
	| argument |
	argument := self parseArgument: 'stringy ''self'''.
	self assertType: argument named: 'stringy'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedStringSelfArgumentIsString [
	| argument |
	argument := self parseArgument: 'stringy ''self'''.
	self assertValue: argument is: 'self'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedTrueArgumentHasDefinedType [
	| argument |
	argument := self parseArgument: 'int64 true'.
	self assertType: argument named: 'int64'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedTrueArgumentIsTrue [
	| argument |
	argument := self parseArgument: 'int true'.
	self assertIsTrue: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedVariableArgumentHasName [
	| argument |
	argument := self parseArgument: 'arbitrary a'.
	self assertIsVariable: argument named: 'a'
]

{ #category : #tests }
FFIFunctionParserTest >> testTypedVariableArgumentHasType [
	| argument |
	argument := self parseArgument: 'arbitrary a'.
	self assertType: argument named: 'arbitrary'
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedConstantIntegerArgumentHasConstantValue [
	| argument |
	argument := self parseArgument: '1'.
	self assertValue: argument is: 1
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedConstantIntegerArgumentHasUndefinedType [
	| argument |
	argument := self parseArgument: '1'.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedConstantNegativeIntegerArgumentHasConstantValue [
	| argument |
	argument := self parseArgument: '-1'.
	self assertValue: argument is: -1
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedConstantNegativeIntegerArgumentHasUndefinedType [
	| argument |
	
	argument := self parseArgument: '-1'.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedFalseArgumentHasUndefinedType [
	| parser argument |
	
	parser := self newParser
		setOn: 'false';
		yourself.

	argument := parser parseArgument.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedFalseArgumentIsFalse [
	| argument |
	
	argument := self parseArgument: 'false'.
	self assertIsFalse: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedNilArgumentHasUndefinedType [
	| argument |
	
	argument := self parseArgument: 'nil'.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedNilArgumentIsNil [
	| argument |
	
	argument := self parseArgument: 'nil'.
	self assertIsNil: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedNullArgumentHasUndefinedType [
	| argument |
	argument := self parseArgument: 'NULL'.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedNullArgumentIsNULL [
	| argument |
	argument := self parseArgument: 'NULL'.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedSelfArgumentHasUndefinedType [
	| argument |
	argument := self parseArgument: 'self'.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedSelfArgumentIsSelf [
	| argument |
	argument := self parseArgument: 'self'.
	self assertIsSelf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedTrueArgumentHasUndefinedType [
	| argument |
	argument := self parseArgument: 'true'.
	self assertUndefinedTypeOf: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testUntypedTrueArgumentIsTrue [
	| argument |
	argument := self parseArgument: 'true'.
	self assertIsTrue: argument
]
