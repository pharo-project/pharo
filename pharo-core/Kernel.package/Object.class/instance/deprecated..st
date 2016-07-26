deprecation
deprecated: anExplanationString
	"Warn that the sending method has been deprecated"
	
	Deprecation new
		method: thisContext sender method;
		explanation: anExplanationString;
		signal