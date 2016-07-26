accessing
link: anODatedEntry
	"Link the given entry after me."

	anODatedEntry
		next: next;
		previous: self.
	next previous: anODatedEntry.
	next := anODatedEntry