instance creation
type: type
	"Create a new entity with a given mime type and (currently) undetermined length"
	
	^ self new
		contentType: type;
		yourself