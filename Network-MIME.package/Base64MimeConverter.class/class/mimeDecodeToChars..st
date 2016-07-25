encoding
mimeDecodeToChars: aStream 
	"Return a ReadWriteStream of the original String.  aStream has only 65 innocuous character values.  It is not binary.  (See class comment). 4 bytes in aStream goes to 3 bytes in output."

	| me |
	aStream position: 0.
	me := self new mimeStream: aStream.
	me dataStream: (ReadWriteStream on: (String new: aStream size * 3 // 4)).
	me mimeDecode.
	me dataStream position: 0.
	^ me dataStream