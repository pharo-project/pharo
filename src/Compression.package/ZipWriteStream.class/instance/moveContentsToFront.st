private
moveContentsToFront
	"Need to update crc here"
	self updateCrc.
	super moveContentsToFront.
	crcPosition := position + 1.