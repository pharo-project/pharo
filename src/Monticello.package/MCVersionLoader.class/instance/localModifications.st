checking
localModifications
	^ versions select: [:ea | ea package workingCopy modified].
	