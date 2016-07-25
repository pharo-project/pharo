initialization
initializeWith: aStore 
	self initialize.
	store := aStore.
	announcer := Announcer new.
	commitByEntryReference := Dictionary new.
	commentByEntryReference := Dictionary new.
	headReference := store headReference