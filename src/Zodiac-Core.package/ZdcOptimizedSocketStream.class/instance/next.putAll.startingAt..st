stream out
next: count putAll: collection startingAt: offset
	"Write count bytes from collection starting at offset. Overwritten, optimized"
	
	| totalWritten |
	totalWritten := 0.
	[ | toWrite |
		toWrite := (count - totalWritten) min: writeBuffer availableForWriting.
		writeBuffer next: toWrite putAll: collection startingAt: offset + totalWritten.
		totalWritten := totalWritten + toWrite.
		totalWritten = count ] whileFalse: [ self flushWriteBuffer ]