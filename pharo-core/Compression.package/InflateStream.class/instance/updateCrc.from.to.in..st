crc
updateCrc: oldCrc from: start to: stop in: aCollection
	"Answer an updated CRC for the range of bytes in aCollection.
	Subclasses can implement the appropriate means for the check sum they wish to use."
	^oldCrc