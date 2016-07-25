accessing
addEntryWith: anEvent tags: blockClosureForCustomTags

	| newEntry |
	newEntry := OmEntry content: anEvent.

	"add tags"
	newEntry tags 
		at: self class priorReferenceKey put: headReference;
		in: blockClosureForCustomTags.

	"write the new entry"
	store newEntry: newEntry.

	"set the new entry as parent"
	headReference := store referenceTo: newEntry.

	"update caches with the new entry"
	self cacheEntry: newEntry.

	self announceAdded: newEntry.

	^ newEntry