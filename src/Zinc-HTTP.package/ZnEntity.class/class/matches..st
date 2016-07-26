testing
matches: mimeType
	"Return whether a given mimeType can be handled by instances of the receiver"
	
	^ mimeType matches: self designatedMimeType 