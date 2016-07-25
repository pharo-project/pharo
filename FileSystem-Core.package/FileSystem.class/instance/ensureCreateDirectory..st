public
ensureCreateDirectory: aResolvable
	"Resolve the argument to an absolute path, then ask the store to make
	sure that all the directories contained in the argument path exist or are created."
	
	store ensureCreateDirectory: (self resolve: aResolvable)