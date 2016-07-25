initialization
getListSelector: sel
	"Set the receiver's getListSelector as indicated, and trigger a recomputation of the list"

	getListSelector := sel.
	self changed.
	self updateList.