operations
checkInitialized
	"Make sure that LigGit2 is properly initialized."

	self ffiLibrary isInitialized 
		ifFalse: [ self ffiLibrary uniqueInstance initializeLibGit2 ]