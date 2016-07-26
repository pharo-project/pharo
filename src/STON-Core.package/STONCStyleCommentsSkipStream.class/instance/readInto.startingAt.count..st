accessing
readInto: collection startingAt: offset count: requestedCount 
	"Read count elements and place them in collection starting at offset.
	Return the number of elements actually read."
	
	^ peekedCharacter 
		ifNil: [ | readCount |
			[ readCount := self encoder 
					readInto: collection 
					startingAt: offset 
					count: requestedCount 
					fromStream: stream ]
				on: ZnByteStringBecameWideString 
				do: [ :byteStringBecameWideString | 
					byteStringBecameWideString becomeForward; resume ].
			readCount ]
		ifNotNil: [ 
			collection at: offset put: peekedCharacter.
			peekedCharacter := nil.
			(self 
				readInto: collection 
				startingAt: offset + 1
				count: requestedCount - 1) + 1 ]