selection
selectedStatementsAsSequence
	| statements temporaries  |
	
	temporaries :=  self selectedTemporaryVariables.
	statements:= self selectedStatements.
	^RBSequenceNode temporaries: temporaries statements: statements
	