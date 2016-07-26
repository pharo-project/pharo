accessing
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available."

	| read |
	read := 0.
	position <= limit
		ifTrue: [ 
			read := (limit - position + 1) min: requestedCount.
			collection replaceFrom: offset to: offset + read - 1 with: buffer startingAt: position.
			position := position + read.
			read = requestedCount ifTrue: [ ^ requestedCount ] ].
	^ read
		+
			(requestedCount - read < (buffer size / 2)
				ifTrue: [ 
					stream atEnd
						ifTrue: [ 0 ]
						ifFalse: [ 
							self
								nextBuffer;
								readInto: collection startingAt: offset + read count: requestedCount - read ] ]
				ifFalse: [ stream readInto: collection startingAt: offset + read count: requestedCount - read ])