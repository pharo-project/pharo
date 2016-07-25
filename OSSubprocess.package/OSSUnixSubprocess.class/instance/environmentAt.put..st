env building
environmentAt: aVariableName put: aValue
	"This is the expected API to allow users to define custom variables and values."
	(aVariableName isString and: [ aValue isString ])
		ifFalse: [ self error: 'Variable names and values must be strings' ].
	envVariables at: aVariableName put: aValue