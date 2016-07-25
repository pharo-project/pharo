registration
registerCallback: aCallback as: functionName
	self callbackRegistry 
		at: functionName asSymbol 
		put: aCallback.
