transforming
abstractVariables
	self performComponentRefactoring: self abstractVariablesRefactoring.
	parseTree := self abstractVariablesRefactoring parseTree