operations
ensureCreateDirectory
	"Verifies that the directory does not exist and only creates if necessary. Do not remove files contained if they exist."
	^ self resolve ensureCreateDirectory