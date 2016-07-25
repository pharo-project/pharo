private
updateCrc
	crcPosition <= position ifTrue:[
		bytesWritten := bytesWritten + position - crcPosition + 1.
		crc := self updateCrc: crc from: crcPosition to: position in: collection.
		crcPosition := position + 1].