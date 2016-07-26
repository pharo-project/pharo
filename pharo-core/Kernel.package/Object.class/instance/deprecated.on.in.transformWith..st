deprecation
deprecated: anExplanationString on: date in: version transformWith: aRule
	"Automatically tranform the deprecated call"
	
	Deprecation new
		method: thisContext sender method;
		explanation: anExplanationString;
		date: date;
		version: version;
		rule: aRule;
		transform.
		