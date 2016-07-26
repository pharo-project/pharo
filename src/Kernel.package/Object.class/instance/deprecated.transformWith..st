deprecation
deprecated: anExplanationString transformWith: aRule
	"Automatically tranform the deprecated call"
	
	Deprecation new
		method: thisContext sender method;
		explanation: anExplanationString;
		rule: aRule;
		transform.
		