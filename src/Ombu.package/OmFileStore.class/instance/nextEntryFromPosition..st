private
nextEntryFromPosition: aFilePosition

	^ self readEntriesWith: [ :readStream |
		readStream position: aFilePosition.
		self newEntryReader stream: readStream; nextEntry ]