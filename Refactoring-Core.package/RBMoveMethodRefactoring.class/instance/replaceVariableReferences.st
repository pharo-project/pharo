transforming
replaceVariableReferences
	| replacer |
	replacer := RBParseTreeRewriter new.
	replacer replace: variable with: 'self'.
	replacer executeTree: parseTree.
	parseTree := replacer tree