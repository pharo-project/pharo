initialization
writeFooter
	"Write some footer information for the crc"
	super writeFooter.
	0 to: 3 do:[:i| encoder nextBytePut: (crc >> (i*8) bitAnd: 255)].
	0 to: 3 do:[:i| encoder nextBytePut: (bytesWritten >> (i*8) bitAnd: 255)].