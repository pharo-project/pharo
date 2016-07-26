crc
updateCrc: oldCrc from: start to: stop in: aCollection
	"Answer an updated CRC for the range of bytes in aCollection"
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection.