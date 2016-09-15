private
newEntryItemFor: anEntry 
	^ EpEntryItem new
		browser: browser;
		entry: anEntry;
		triggerItems: (self triggerItemsFor: anEntry);
		yourself