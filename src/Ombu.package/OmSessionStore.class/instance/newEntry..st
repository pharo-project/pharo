writing
newEntry: anEntry
	"Persist a new entry"

	self store newEntry: anEntry.
	headReference := self store headReference.