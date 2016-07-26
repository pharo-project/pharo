initialize-release
initialize
	super initialize.
	hasUnacceptedEdits := false.
	hasEditingConflicts := false.
	askBeforeDiscardingEdits := true