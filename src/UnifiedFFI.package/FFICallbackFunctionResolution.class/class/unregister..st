registration
unregister: functionName
	self callbackRegistry removeKey: functionName asSymbol 
		