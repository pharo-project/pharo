crc
expectedCrc: aNumberOrNil
	"If expectedCrc is set, it will be compared against the calculated CRC32 in verifyCrc.
	This number should be the number read from the Zip header (which is the bitwise complement of my crc if all is working correctly)"
	expectedCrc := aNumberOrNil