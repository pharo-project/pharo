preconditions
checkVariableReferencesIn: aParseTree 
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: '`var'
		do: 
			[:aNode :answer | 
			| name |
			name := aNode name.
			(aNode whoDefines: name) isNil ifTrue: [self checkSendersAccessTo: name]].
	searcher executeTree: aParseTree