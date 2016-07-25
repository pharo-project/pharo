initialize-release
method: aMethod class: aClass literals: literals

	method := aMethod.
	instVars := aClass allInstVarNames.
	nArgs := method numArgs.
	literalValues := literals