stream out
next: count putAll: collection startingAt: offset
	"Write count bytes from collection starting at offset. Overwritten, optimized"
	
	count <= (writeBuffer bufferSize / 8)  
		ifTrue: [
			count > writeBuffer availableForWriting ifTrue: [ self flushWriteBuffer ].  
			writeBuffer next: count putAll: collection startingAt: offset ]
		ifFalse: [
			self flushWriteBuffer.
			self flushBytes: collection startingAt: offset count: count ]