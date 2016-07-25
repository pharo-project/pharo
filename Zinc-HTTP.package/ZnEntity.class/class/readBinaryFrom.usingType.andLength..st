instance creation
readBinaryFrom: stream usingType: mimeType andLength: length
	"Create and return a new buffered entity of mimeType and length from stream.
	The concrete subclass depends on mimeType, but textual type will remain binary"
	
	| newEntity |
	newEntity := (self concreteSubclassForType: mimeType binary: true) 
						type: mimeType length: length.
	newEntity readBinaryFrom: stream.
	^ newEntity 
