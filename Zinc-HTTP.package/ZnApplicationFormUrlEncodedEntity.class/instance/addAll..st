accessing
addAll: keyedCollection
	"Note that we use #addAllMulti:"

	self invalidateRepresentation.
	self fields addAllMulti: keyedCollection.
	^ keyedCollection 