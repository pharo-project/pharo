initialization
on: aCollectionOrStream
	crc := 16rFFFFFFFF.
	crcPosition := 1.
	bytesWritten := 0.
	encoder := ZipEncoder on: aCollectionOrStream.
	encoder isBinary
		ifTrue:[super on: ByteArray new]
		ifFalse:[super on: String new].
	self writeHeader.
