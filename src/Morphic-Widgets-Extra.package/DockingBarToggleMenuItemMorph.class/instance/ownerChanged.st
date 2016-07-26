accessing
ownerChanged
	"The receiver's owner, some kind of a pasteup, has changed its 
	layout."
	super ownerChanged.
	self updateLayoutInDockingBar