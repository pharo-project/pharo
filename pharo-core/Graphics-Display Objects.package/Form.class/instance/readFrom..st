filein/out
readFrom: aBinaryStream
	"Reads the receiver from the given binary stream with the format:
		depth, extent, offset, bits."
	self readAttributesFrom: aBinaryStream.
	self readBitsFrom: aBinaryStream