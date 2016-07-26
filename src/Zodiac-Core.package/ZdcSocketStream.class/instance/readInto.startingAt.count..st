stream in
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read. Overwritten, optimized"
	
	| totalRead read |
	totalRead := 0.
	(read := readBuffer availableForReading min: requestedCount) > 0
		ifTrue: [  
			collection replaceFrom: offset to: offset + read - 1 with: readBuffer buffer startingAt: readBuffer contentsStart.
			readBuffer advanceReadPointer: read.
			(totalRead := totalRead + read) = requestedCount ifTrue: [ ^ totalRead ] ].  
	[
		read := self fillBytes: collection startingAt: offset + totalRead count: requestedCount - totalRead.
		(totalRead := totalRead + read) = requestedCount or: [ self isConnected not ] ] whileFalse: [ 
			self socketWaitForData ].
	^ totalRead