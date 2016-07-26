private
updateEntriesCache

	headReference := store headReference.
	store entriesDo: [ :each | self cacheEntry: each ]