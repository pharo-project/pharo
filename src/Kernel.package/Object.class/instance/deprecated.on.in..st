deprecation
deprecated: anExplanationString on: date in: version
	"Warn that the sending method has been deprecated"
	
	Deprecation new
		method: thisContext sender method;
		sendingMethodName: thisContext sender sender method printString;
		explanation: anExplanationString;
		date: date;
		version: version;
		signal