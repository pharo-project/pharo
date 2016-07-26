private
newFetchMoreItemFor: anEntryReference in: aCollection
	^ EpFetchMoreItem new
		browser: browser;
		container: aCollection;
		entryReference: anEntryReference;
		yourself