"
Tests for FFIFunctionParser
"
Class {
	#name : #FFIFunctionParserTest,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #asserting }
FFIFunctionParserTest >> assertIsFalse: anArgument [
	
	self assert: anArgument third equals: 'false'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsNil: anArgument [
	
	self assert: anArgument third equals: 'nil'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsSelf: anArgument [
	
	self assert: anArgument third equals: 'self'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsTrue: anArgument [
	
	self assert: anArgument third equals: 'true'
]

{ #category : #asserting }
FFIFunctionParserTest >> assertIsVariable: argument named: aName [

	self assert: argument first equals: aName
]

{ #category : #asserting }
FFIFunctionParserTest >> assertType: anArgument named: aTypeName [
	
	self assert: anArgument third equals: aTypeName
]

{ #category : #asserting }
FFIFunctionParserTest >> assertValue: aLiteralArgument is: aValue [
	
	self assert: aLiteralArgument equals: aValue
]

{ #category : #factory }
FFIFunctionParserTest >> newParser [
	^ FFIFunctionParser new
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
		self assert: parser returnType equals: #('int' 2).
		
		args := parser arguments.
		self assert: args size equals: 8.

		self assert: (args at: 1) equals: 0. 
		self assert: (args at: 2) equals: #(nil nil 'nil' 0). 
		self assert: (args at: 3) equals: -10. 
		self assert: (args at: 4) equals: #(nil nil 'FOO_BAR' 0). 
		self assert: (args at: 5) equals: #('a' nil 'int' 0). 
		self assert: (args at: 6) equals: #('_b' nil 'int' 1). 
		self assert: (args at: 7) equals: #('c' nil 'char' 2). 
		self assert: (args at: 8) equals: #('d_' nil 'void' 3 ) ]
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
		self assert: parser returnType equals: #('int' 2).
	
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
		self assert: parser returnType equals: #('int' 2).
		
		args := parser arguments.
		self assert: args size equals: 8.

		self assert: (args at: 1) equals: 0. 
		self assert: (args at: 2) equals: #(nil nil 'nil' 0). 
		self assert: (args at: 3) equals: -10. 
		self assert: (args at: 4) equals: #(nil nil 'FOO_BAR' 0). 
		self assert: (args at: 5) equals: #('a' nil 'int' 0). 
		self assert: (args at: 6) equals: #('_b' nil 'int' 1). 
		self assert: (args at: 7) equals: #('c' nil 'char' 2). 
		self assert: (args at: 8) equals: #('d_' nil 'void' 3 )
	].

]

{ #category : #tests }
FFIFunctionParserTest >> testParseFunctionNoArguments [
	| parser args |
	
	#((int * * function_name ( void ) )
	  ' int * * function_name  ( void ) ') 
	do: [:spec |
		parser := self newParser parseNamedFunction: spec.
		
		self assert: parser functionName equals: 'function_name'.
		self assert: parser returnType equals: #('int' 2).
	
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
FFIFunctionParserTest >> testUntypedConstantNegativeIntegerArgumentHasConstantValue [
	| parser argument |
	
	parser := self newParser
		setStream: '-1' readStream;
		yourself.

	argument := parser parseArgument.
	self assertValue: argument is: -1
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
FFIFunctionParserTest >> testUntypedNilArgumentIsNil [
	| parser argument |
	
	parser := self newParser
		setStream: 'nil' readStream;
		yourself.

	argument := parser parseArgument.
	self assertIsNil: argument
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
FFIFunctionParserTest >> testUntypedTrueArgumentIsTrue [
	| parser argument |
	
	parser := self newParser
		setStream: 'true' readStream;
		yourself.

	argument := parser parseArgument.
	self assertIsTrue: argument
]

{ #category : #tests }
FFIFunctionParserTest >> testVariableArgumentReturnsVariable [
	| parser argument |
	
	parser := self newParser
		setStream: 'arbitrary a' readStream;
		yourself.
	
	argument := parser parseArgument.
	self assertIsVariable: argument named: 'a'
]
