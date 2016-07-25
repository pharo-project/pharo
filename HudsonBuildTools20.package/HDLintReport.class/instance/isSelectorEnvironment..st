testing
isSelectorEnvironment: anEnvironment
	^ #(SelectorEnvironment ParseTreeEnvironment VariableEnvironment) includes: anEnvironment class name