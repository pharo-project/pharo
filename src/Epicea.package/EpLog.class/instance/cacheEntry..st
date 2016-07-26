private
cacheEntry: newEntry
	"Update caches with a new entry"

	newEntry content isEpLogEntriesComment ifTrue: [ 
		(newEntry content entryReferences) do: [ :each |
			commentByEntryReference 
				at: each
				put: newEntry content comment 
		] ]