class initialization
byteToUnicodeSpec
	"Sepcify a table mapping the entries 0x80 to 0xFF to their unicode counterparts by returning a 128 element array..
	The entries 0x00 to 0x7F map to identical values so we don't need to specify them.
	See #generateByteToUnicodeSpec: for an automated way to generate these mappings."

	self subclassResponsibility