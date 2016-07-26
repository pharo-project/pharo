private
newAbsentItemFor: anEntryReference in: aCollection
	^ EpAbsentItem new
		browser: browser;
		container: aCollection;
		entryReference: anEntryReference;
		yourself