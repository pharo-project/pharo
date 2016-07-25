correcting
possibleVariablesFor: proposedVariable 
	| results class |
	class := node methodNode methodClass .
	
	results := proposedVariable correctAgainst: node temporaryVariables
								continuedFrom: nil.
	proposedVariable first canBeGlobalVarInitial ifTrue:
		[ results := class possibleVariablesFor: proposedVariable
						continuedFrom: results ].
	^ proposedVariable correctAgainst: nil continuedFrom: results.
