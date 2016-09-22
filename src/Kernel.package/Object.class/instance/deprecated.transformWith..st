deprecation
deprecated: anExplanationString transformWith: aRule
	"Automatically tranform the deprecated call"
	
	Deprecation new
		method: thisContext sender method;
		sendingMethodName: thisContext sender sender method printString;
		explanation: anExplanationString;
		rule: aRule;
		transform.
		