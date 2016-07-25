history
removeFromHistory: aNautilusHistoryEntry

	self browsingHistory removeEntry: aNautilusHistoryEntry.
	self historyChanged