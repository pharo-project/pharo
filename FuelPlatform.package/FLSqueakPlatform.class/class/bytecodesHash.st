private-extensions-compiledmethod
bytecodesHash
	<flExtensionOf: #CompiledMethod>
	"Answer a 16-bit checksum of the bytecodes."

	^ self crc16from: self initialPC to: self endPC