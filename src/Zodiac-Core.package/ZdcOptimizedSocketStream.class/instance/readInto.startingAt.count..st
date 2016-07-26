stream in
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read. Overwritten, optimized"
	
	| totalRead |
	totalRead := 0.
	[ | read |
		(read := readBuffer availableForReading min: (requestedCount - totalRead)) > 0
			ifTrue: [  
				collection 
					replaceFrom: offset + totalRead to: offset + totalRead + read - 1 
					with: readBuffer buffer startingAt: readBuffer contentsStart.
				readBuffer advanceReadPointer: read.
				totalRead := totalRead + read ].
		totalRead = requestedCount or: [ self isConnected not ] ] whileFalse: [ self fillReadBuffer ].   
	^ totalRead