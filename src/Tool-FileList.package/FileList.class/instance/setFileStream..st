initialization
setFileStream: aStream
	"Used to initialize a spawned file editor.  Sets directory too."
	self directory: aStream name asFileReference.
	pattern := nil.
	aStream close.
	brevityState := #needToGetBrief.
	self changed: #fileList.
	self changed: #contents.
	self changed: #pattern.
	self changed: #selectedDirectory.
