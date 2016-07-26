initialization
initialize
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher
		matchesArgument: '`var'
		do: [ :aNode :answer | 
			answer or: [
				varName := aNode name.
				vars includes: varName ]]