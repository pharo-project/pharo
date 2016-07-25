encoding
mimeDecodeToBytes: aStream 
	"Return a RWBinaryOrTextStream of the original ByteArray.  aStream has only 65 innocuous character values.  aStream is not binary.  (See class comment). 4 bytes in aStream goes to 3 bytes in output."

	| me |
	aStream position: 0.
	me := self new mimeStream: aStream.
	me dataStream: (RWBinaryOrTextStream on: (ByteArray new: aStream size * 3 // 4)).
	me mimeDecodeToByteArray.
	me dataStream position: 0.
	^ me dataStream