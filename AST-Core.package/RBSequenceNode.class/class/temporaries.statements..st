instance creation
temporaries: variableNodes statements: statementNodes 
	^(self new)
		temporaries: variableNodes;
		statements: statementNodes;
		yourself