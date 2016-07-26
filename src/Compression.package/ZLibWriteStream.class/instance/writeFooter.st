initialization
writeFooter
	"Store the Adler32 checksum as the last 4 bytes."
	3 to: 0 by: -1 do:[:i| encoder nextBytePut: (crc >> (i*8) bitAnd: 255)].