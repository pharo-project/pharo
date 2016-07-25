private
modifyImplementorParseTree: parseTree in: aClass 
	| name newArg allTempVars |
	allTempVars := parseTree allDefinedVariables.
	name := self safeVariableNameFor: aClass temporaries: allTempVars.
	newArg := RBVariableNode named: name.
	parseTree 
		renameSelector: newSelector
		andArguments: parseTree arguments , (Array with: newArg)