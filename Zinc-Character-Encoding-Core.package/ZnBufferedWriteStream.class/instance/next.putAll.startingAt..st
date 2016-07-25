accessing
next: count putAll: collection startingAt: offset
	"Write count elements from collection starting at offset."
	
	self flushBufferIfFull.
	count <= self bufferFreeSize
		ifTrue: [
			self buffer replaceFrom: position + 1 to: position + count with: collection startingAt: offset.
			position := position + count ]
		ifFalse: [
			self flushBuffer.
			count > (self bufferSize / 2)
				ifTrue: [ stream next: count putAll: collection startingAt: offset ]
				ifFalse: [ self next: count putAll: collection startingAt: offset ] ]
