transforming
computePoolsToMove
	| poolVariables searcher |
	poolVariables := self poolVariableNamesFor: fromClass.
	pools := Set new.
	searcher := RBParseTreeSearcher new.
	searcher matches: '`var'
		do: 
			[:aNode :answer | 
			| varName pool |
			varName := aNode name.
			(aNode whoDefines: varName) isNil 
				ifTrue: 
					[(poolVariables includes: varName) 
						ifTrue: 
							[pool := self whichPoolDefines: varName.
							pool notNil ifTrue: [pools add: pool]]]].
	searcher executeTree: parseTree