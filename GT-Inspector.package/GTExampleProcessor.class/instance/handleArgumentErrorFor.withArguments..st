private
handleArgumentErrorFor: anExample withArguments: arguments
	^ (GTExampleArgumentError new
		example: anExample;
		expected: anExample method argumentNames;
		defined: anExample arguments;
		evaluated: arguments;
		yourself) signal