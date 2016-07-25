morphic ui
saveChanges
	"Ensure we do refresh. A git repository is a local beast, so it is ok to call a refresh."

	super saveChanges.
	self refresh