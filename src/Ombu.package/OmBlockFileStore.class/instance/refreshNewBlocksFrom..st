refreshing
refreshNewBlocksFrom: readStream

	| reader blockPosition blockFirstEntry |
	reader := self newEntryReader.
	reader stream: readStream.
	
	[	blockPosition := blocks size * self blockSize.
		blockPosition < readStream size ] whileTrue: [
			readStream position: blockPosition.
			reader nextEntryPositionIfFound: [:position | 
				readStream position: position.
				blockFirstEntry := reader nextEntry.
				blocks add: (OmBlock
					store: self
					startPosition: position
					endPosition: blockPosition + self blockSize - 1
					firstEntryReference: (self referenceTo: blockFirstEntry)).
				] ifNone: [ ^self "No more entries to add in a new block" ].
		]
