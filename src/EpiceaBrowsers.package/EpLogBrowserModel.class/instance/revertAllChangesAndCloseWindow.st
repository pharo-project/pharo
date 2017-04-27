menu - operations
revertAllChangesAndCloseWindow

	self revertChangesIn: self entriesWithCodeChanges.
	self window close.
