accessing
nextPutAll: collection
	self flushBufferIfFull.
	collection size <= self bufferFreeSize
		ifTrue: [
			self buffer replaceFrom: position + 1 to: position + collection size with: collection.
			position := position + collection size ]
		ifFalse: [
			self flushBuffer.
			collection size > (self buffer size / 2)
				ifTrue: [ stream nextPutAll: collection ]
				ifFalse: [ self nextPutAll: collection ] ]
