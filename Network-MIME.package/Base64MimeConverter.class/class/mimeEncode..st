encoding
mimeEncode: aStream
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."

	^self mimeEncode: aStream multiLine: true