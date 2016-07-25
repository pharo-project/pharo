instance creation
with: object
	"Create a new entity holding object.
	Currently works for strings and byte arrays.
	You will probably want to set the mime type to something more specific"
	
	^ object isString 
		ifTrue: [ self text: object ] 
		ifFalse: [ self bytes: object ]