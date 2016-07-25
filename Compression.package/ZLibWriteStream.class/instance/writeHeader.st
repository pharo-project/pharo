initialization
writeHeader
	"Write header information"
	encoder nextBits: 8 put: 120. "deflate method with 15bit window size"
	encoder nextBits: 8 put: 94. "checksum; no preset; fast (flevel=1) compression"