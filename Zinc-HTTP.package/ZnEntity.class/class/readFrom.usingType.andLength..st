instance creation
readFrom: stream usingType: mimeType andLength: length
	"Create and return a new buffered entity of mimeType and length from stream.
	The concrete subclass depends on mimeType"
	
	| newEntity |
	newEntity := (self concreteSubclassForType: mimeType binary: false) 
						type: mimeType length: length.
	newEntity readFrom: stream.
	^ newEntity 
