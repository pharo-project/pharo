testing
testPragmaUnarayMessage
	| aRBMethode |

	aRBMethode := OpalCompiler new parse: self methodPragmaUnarayMessage.
	
	self assert: (aRBMethode compiledMethod pragmas first keyword = #hello)