private
decompressAll
	"Profile the decompression speed"
	[self atEnd] whileFalse:[
		position := readLimit.
		self next "Provokes decompression"
	].