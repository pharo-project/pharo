Class {
	#name : #FFIFunctionParserTests,
	#superclass : #TestCase,
	#category : 'UnifiedFFI-Tests'
}

{ #category : #factory }
FFIFunctionParserTests >> newParser [
	^ FFIFunctionParser new
]

{ #category : #tests }
FFIFunctionParserTests >> testParseAnonymousFunction [
	 | parser args |
	
	#((int * * ( 0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_))
	  ' int * * (0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_    ' ) 
	do: [:spec |
		parser := self newParser parseAnonymousFunction: spec.
		
		self assert: parser isAnonymous.
		self assert: parser functionName isNil.
		self assert: parser returnType = #('int' 2).
		
		args := parser arguments.
		self assert: args size = 8.

		self assert: (args at: 1) = 0. 
		self assert: (args at: 2) = #(nil nil 'nil' 0). 
		self assert: (args at: 3) = -10. 
		self assert: (args at: 4) = #(nil nil 'FOO_BAR' 0). 
		self assert: (args at: 5) = #('a' nil 'int' 0). 
		self assert: (args at: 6) = #('_b' nil 'int' 1). 
		self assert: (args at: 7) = #('c' nil 'char' 2). 
		self assert: (args at: 8) = #('d_' nil 'void' 3 ) ]
]

{ #category : #tests }
FFIFunctionParserTests >> testParseAnonymousFunctionNoArguments [
	| parser args |
	
	#((int * * ( void ) )
	  ' int * * ( void ) ') 
	do: [:spec |
		parser := self newParser parseAnonymousFunction: spec.
		
		self assert: parser isAnonymous.
		self assert: parser functionName isNil.
		self assert: parser returnType = #('int' 2).
	
		args := parser arguments.
		self assert: args size = 0 ]
]

{ #category : #tests }
FFIFunctionParserTests >> testParseFunction [
	 | parser args |
	
	#((int * * #'function_n$a$m$e' ( 0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_))
	  ' int * * function_n$a$m$e (0, nil, -10, FOO_BAR , int a, int* _b, char** c, void* * * d_    ' ) 
	do: [:spec |
		parser := self newParser parseNamedFunction: spec.
		
		self assert: parser functionName = 'function_n$a$m$e'.
		self assert: parser returnType = #('int' 2).
		
		args := parser arguments.
		self assert: args size = 8.

		self assert: (args at: 1) = 0. 
		self assert: (args at: 2) = #(nil nil 'nil' 0). 
		self assert: (args at: 3) = -10. 
		self assert: (args at: 4) = #(nil nil 'FOO_BAR' 0). 
		self assert: (args at: 5) = #('a' nil 'int' 0). 
		self assert: (args at: 6) = #('_b' nil 'int' 1). 
		self assert: (args at: 7) = #('c' nil 'char' 2). 
		self assert: (args at: 8) = #('d_' nil 'void' 3 ). 
	].

]

{ #category : #tests }
FFIFunctionParserTests >> testParseFunctionNoArguments [
	| parser args |
	
	#((int * * function_name ( void ) )
	  ' int * * function_name  ( void ) ') 
	do: [:spec |
		parser := self newParser parseNamedFunction: spec.
		
		self assert: parser functionName = 'function_name'.
		self assert: parser returnType = #('int' 2).
	
		args := parser arguments.
		self assert: args size = 0 ]
]
