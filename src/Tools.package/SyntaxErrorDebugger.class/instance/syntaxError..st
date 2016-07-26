initialization
syntaxError: aSyntaxError
	"extract the instance properties from a SyntaxErrorNotification"
	
	syntaxError := aSyntaxError.
	self 
		setClass: aSyntaxError errorClass
		code: aSyntaxError errorCode
		error: aSyntaxError errorMessage
		location: aSyntaxError location
		debugSession: (Processor activeProcess newDebugSessionNamed: 'Stack of the Syntax Error' startedAt: aSyntaxError signalerContext)
		doitFlag: aSyntaxError doitFlag.
		
	self category: aSyntaxError category.