initialization
writeHeader
	"Write the GZip header"
	encoder nextBits: 16 put: GZipMagic.
	encoder nextBits: 8 put: GZipDeflated.
	encoder nextBits: 8 put: 0. "No flags"
	encoder nextBits: 32 put: 0. "no time stamp"
	encoder nextBits: 8 put: 0. "No extra flags"
	encoder nextBits: 8 put: 0. "No OS type"
