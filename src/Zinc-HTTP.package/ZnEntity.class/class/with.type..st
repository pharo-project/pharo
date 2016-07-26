instance creation
with: object type: type
	"Create a new entity holding object.
	Currently works for strings and byte arrays.
	The entity is set to mime type"
	
	^ (self with: object)
		contentType: type;
		yourself