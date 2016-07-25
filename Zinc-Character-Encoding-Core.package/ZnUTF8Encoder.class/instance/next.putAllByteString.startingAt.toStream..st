private
next: count putAllByteString: string startingAt: offset toStream: stream
	"A faster version when string is a ByteString"
	
	| lastIndex nextIndex |
	lastIndex := offset.
	nextIndex := self findFirstNonASCIIIn: string startingAt: lastIndex.
	(nextIndex = 0 or: [ offset + count <= nextIndex ]) 
		ifTrue: [ 
			^ self next: count putAllASCII: string startingAt: offset toStream: stream ].
	[	nextIndex > lastIndex 
			ifTrue: [
				self next: nextIndex - lastIndex putAllASCII: string startingAt: lastIndex toStream: stream ].
		stream nextPutAll: (ByteUTF8Encoding at: (string byteAt: nextIndex) + 1).
		lastIndex := nextIndex + 1.
		nextIndex := self findFirstNonASCIIIn: string startingAt: lastIndex.
		nextIndex = 0 or: [ offset + count <= nextIndex ] ] whileFalse.
	offset + count <= lastIndex 
		ifFalse: [ 
			self next: offset + count - lastIndex putAllASCII: string startingAt: lastIndex toStream: stream ]
	