transforming
renameConflictingTemporaries
	inlineParseTree allDefinedVariables 
		do: [:each | self renameConflictingTemporary: each]