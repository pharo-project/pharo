testing
isClassEnvironment: anEnvironment
	^ #(CategoryEnvironment ClassEnvironment VariableEnvironment) includes: anEnvironment class name