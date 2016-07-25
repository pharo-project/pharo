file system
ensureDirectoryExists: aDirectoryHandle
	"Ensure the directory exists."

	^ aDirectoryHandle ensureCreateDirectory ; yourself