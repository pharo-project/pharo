accessing
entryPositionsStartingAt: startPosition upTo: endPosition

	self readEntriesWith: [:readStream |
		readStream position: startPosition.
		^self newEntryReader
			stream: readStream;
			entryPositionsUpTo: endPosition ]