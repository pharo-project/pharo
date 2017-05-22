initialization
initialize
	super initialize.
	
	"Options are only ever used during a single setup function call.
	The contents of the struct are then copied to a libgit2 internal
	struct."
	self autoRelease