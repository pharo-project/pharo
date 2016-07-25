execution
isValidForContext
	^self selectedNodeHasBreakpoint not and: [self hasUnacceptedEdits not and:[ super isValidForContext ]]