deprecation
deprecated: anExplanationString transformWith: aRule
	"Automatically tranform the deprecated call"
	
	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		rule: aRule;
		transform.
		