accessing request
accept: mimeTypeObject
	"Set the Accept HTTP header to mimeTypeObject for the current request."

	| mimeType |
	mimeType := mimeTypeObject asZnMimeType.
	self request setAccept: mimeType