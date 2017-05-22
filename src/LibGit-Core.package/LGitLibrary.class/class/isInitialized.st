testing
isInitialized
	"Answer true when git_libgit2_init has been called in the current session."

	^ Initialized ifNil: [ false ]