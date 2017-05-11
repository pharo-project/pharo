accessing
filteredEntries
	
	^ self filteredEntryReferences collect: [:each | log entryFor: each ]