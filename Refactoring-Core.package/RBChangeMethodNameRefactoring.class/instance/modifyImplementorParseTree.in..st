private
modifyImplementorParseTree: parseTree in: aClass 
	| oldArgs |
	oldArgs := parseTree arguments.
	parseTree renameSelector: newSelector andArguments: (permutation collect: [:each | oldArgs at: each]) 