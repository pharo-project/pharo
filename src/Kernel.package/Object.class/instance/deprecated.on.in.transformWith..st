deprecation
deprecated: anExplanationString on: date in: version transformWith: aRule
	"Automatically tranform the deprecated call"
	
	Deprecation new
		context: thisContext sender;
		explanation: anExplanationString;
		date: date;
		version: version;
		rule: aRule;
		transform.
		