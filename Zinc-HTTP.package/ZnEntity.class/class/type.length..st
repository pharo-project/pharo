instance creation
type: type length: length
	"Create a new entity with a given mime type and length"
	
	^ self new
		contentType: type;
		contentLength: length;
		yourself