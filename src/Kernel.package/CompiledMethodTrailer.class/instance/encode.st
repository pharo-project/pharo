encoding
encode

	encodedData := nil.
	
	"encode the trailer into byte array"
	self perform: ('encode' , kind) asSymbol.

	[encodedData notNil and: [encodedData size > 0 ]] assert.
	
	"set the size"
	size := encodedData size.