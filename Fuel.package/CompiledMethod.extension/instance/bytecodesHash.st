*Fuel
bytecodesHash
	"Answer a 16-bit checksum of the bytecodes."

	^ CRC crc16FromCollection: (ByteArray 
		new: self size 
		streamContents: [ :stream | self 
			from: self initialPC 
			to: self endPC 
			do: [ :byte | stream nextPut: byte ] ])