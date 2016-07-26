crc
updateCrc: oldCrc from: start to: stop in: aCollection
	"Answer an updated CRC for the range of bytes in aCollection"
	^ Adler32 update: oldCrc from: start to: stop in: aCollection