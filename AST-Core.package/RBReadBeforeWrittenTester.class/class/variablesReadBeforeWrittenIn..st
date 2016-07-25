accessing
variablesReadBeforeWrittenIn: aParseTree 
	^(self new)
		executeTree: aParseTree;
		read