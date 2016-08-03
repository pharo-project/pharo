private
newFilteredItemFor: anEntry 
	^ EpFilteredEntriesItem new
		browser: browser;
		olderFilteredEntry: anEntry;
		yourself