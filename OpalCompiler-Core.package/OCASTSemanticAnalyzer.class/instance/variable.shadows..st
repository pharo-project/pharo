error handling
variable: variableNode shadows: semVar

	^ OCShadowVariableWarning new
		node: variableNode;
		shadowedVar: semVar;
		compilationContext: compilationContext;
		signal