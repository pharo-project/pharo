private
newFilteredItemFor: anEntry 
	^ EpFilteredEntriesItem new
		browser: browser;
		entries: { anEntry };
		yourself