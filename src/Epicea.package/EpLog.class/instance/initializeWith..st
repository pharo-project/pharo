initialization
initializeWith: aStore 
	self initialize.
	store := aStore.
	announcer := Announcer new.
	commentByEntryReference := Dictionary new.
	headReference := store headReference