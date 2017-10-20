operations
ensureCreateDirectory
	"Verifies that the directory does not exist and only creates if necessary. Do not remove files contained if they exist.Creates the parents if needed"
	^ self resolve ensureCreateDirectory