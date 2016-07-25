writing
writeChunk: crcStream 
	| bytes length crc debug |
	debug := self debugging.
	bytes := crcStream originalContents.
	length := crcStream position.
	crc := self 
		updateCrc: 4294967295
		from: 1
		to: length
		in: bytes.
	crc := crc bitXor: 4294967295.
	debug ifTrue: 
		[ Transcript
			cr;
			print: stream position;
			space;
			nextPutAll: (bytes 
					copyFrom: 1
					to: 4) asString;
			nextPutAll: ' len=';
			print: length;
			nextPutAll: ' crc=0x';
			nextPutAll: crc printStringHex ].
	stream 
		nextNumber: 4
		put: length - 4.	"exclude chunk name"
	stream 
		next: length
		putAll: bytes
		startingAt: 1.
	stream 
		nextNumber: 4
		put: crc.
	debug ifTrue: 
		[ Transcript
			nextPutAll: ' afterPos=';
			print: stream position ].
	crcStream resetToStart