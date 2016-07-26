options
contentReader: object
	"Set the content reader, an object conforming 
	to the #value: protocol to convert a response entity
	as used by #contents to object.
	Can be used to parse incoming data."

	self optionAt: #contentReader put: object