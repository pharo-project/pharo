accessing
readBeforeWritten: varNames in: aParseTree 
	^(self new)
		checkNewTemps: false;
		initializeVars: varNames;
		executeTree: aParseTree;
		read