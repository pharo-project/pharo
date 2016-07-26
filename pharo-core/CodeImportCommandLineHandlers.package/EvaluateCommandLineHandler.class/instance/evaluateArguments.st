activation
evaluateArguments
	| argumentString |
	argumentString :=  self argumentString.
	argumentString ifEmpty: [ ^ self ].
	self evaluate: argumentString