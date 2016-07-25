private-writing
compressDataTo: aStream
	"Copy my deflated data to the given stream."
	| encoder startPos endPos |

	encoder := ZipWriteStream on: aStream.
	startPos := aStream position.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		encoder nextPutAll: data asByteArray.
		readDataRemaining := readDataRemaining - data size.
	].
	encoder finish. "not close!"
	endPos := aStream position.
	compressedSize := endPos - startPos.
	crc32 := encoder crc.
