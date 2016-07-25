private-changes
replaceClassNameIn: definitionString to: aSymbol 
	| parseTree |
	parseTree := RBParser parseExpression: definitionString.
	parseTree receiver: (RBVariableNode named: aSymbol).
	^parseTree formattedCode