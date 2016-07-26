writing
nextBytesPutAll: collection
	self flushBufferIfFull.
	collection size <= self bufferFreeSize
		ifTrue: [
			self buffer replaceFrom: position + 1 to: position + collection size with: collection.
			position := position + collection size ]
		ifFalse: [
			self flushBuffer.
			collection size > (self buffer size / 2)
				ifTrue: [ stream nextBytesPutAll: collection ]
				ifFalse: [ self nextBytesPutAll: collection ] ]
