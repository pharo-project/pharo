deprecation
deprecated: anExplanationString
	"Warn that the sending method has been deprecated"
	
	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		signal