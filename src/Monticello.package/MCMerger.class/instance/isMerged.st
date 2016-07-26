testing
isMerged
	^ self conflicts allSatisfy: [:ea | ea isResolved]