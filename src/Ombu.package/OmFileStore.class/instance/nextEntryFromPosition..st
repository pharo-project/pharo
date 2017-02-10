private
nextEntryFromPosition: aFilePosition

	^ self readEntriesWith: [ :readStream |
		readStream position: aFilePosition.
		self entryReader nextEntryFrom: readStream ]