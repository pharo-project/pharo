running-helpers
matcherFor: aVarName


	^ RBParseTreeSearcher new 
		matches: aVarName , ' := `#object'
		do: [ :aNode :answer | aNode ];
		yourself
