deprecation
deprecated: anExplanationString
	"Warn that the sending method has been deprecated"
	
	Deprecation new
		method: thisContext sender method;
		sendingMethodName: thisContext sender sender method printString;
		explanation: anExplanationString;
		signal