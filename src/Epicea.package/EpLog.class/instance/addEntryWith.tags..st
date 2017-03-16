accessing
addEntryWith: anEvent tags: blockClosureForCustomTags
	"Add an event with the specified tags"

	| newEntry |
	newEntry := OmEntry content: anEvent.

	"add tags"
	newEntry tags 
		at: self class priorReferenceKey put: self headReference;
		in: blockClosureForCustomTags.

	"write the new entry"
	store newEntry: newEntry.

	"update caches with the new entry"
	self cacheEntry: newEntry.

	self announceAdded: newEntry.

	^ newEntry