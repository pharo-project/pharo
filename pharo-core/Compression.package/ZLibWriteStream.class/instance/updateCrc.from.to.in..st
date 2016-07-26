initialization
updateCrc: adler from: start to: stop in: aCollection
	"Update crc using the Adler32 checksum technique from RFC1950"
	^ Adler32 update: adler from:  start to:  stop in: aCollection