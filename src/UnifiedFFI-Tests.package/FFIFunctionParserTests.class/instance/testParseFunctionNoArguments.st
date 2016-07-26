tests
testParseFunctionNoArguments
	| parser args |
	
	#((int * * function_name ( void ) )
	  ' int * * function_name  ( void ) ') 
	do: [:spec |
		parser := self newParser parseNamedFunction: spec.
		
		self assert: parser functionName = 'function_name'.
		self assert: parser returnType = #('int' 2).
	
		args := parser arguments.
		self assert: args size = 0 ]