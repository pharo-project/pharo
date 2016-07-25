encoding
mimeEncode: aStream multiLine: aBool
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."

	| me |
	me := self new dataStream: aStream.
	me multiLine: aBool.
	me mimeStream: (ReadWriteStream on: (String new: aStream size + 20 * 4 // 3)).
	me mimeEncode.
	me mimeStream position: 0.
	^ me mimeStream