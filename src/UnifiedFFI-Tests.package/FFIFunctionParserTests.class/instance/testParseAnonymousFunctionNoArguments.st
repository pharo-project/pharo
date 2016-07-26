tests
testParseAnonymousFunctionNoArguments
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