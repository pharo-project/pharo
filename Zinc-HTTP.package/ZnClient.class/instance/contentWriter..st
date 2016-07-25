options
contentWriter: object
	"Set current content writer, an object conforming 
	to the #value: protocol to convert an object to a request content entity
	as used by #contents:. Defaults to nil.
	Can be used to format data to be uploaded."

	self optionAt: #contentWriter put: object