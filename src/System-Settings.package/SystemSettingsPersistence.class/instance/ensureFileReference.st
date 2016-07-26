accessing
ensureFileReference
	"It ensures that the parent directory exists."
	self fileReference parent ensureCreateDirectory.
	^ self fileReference