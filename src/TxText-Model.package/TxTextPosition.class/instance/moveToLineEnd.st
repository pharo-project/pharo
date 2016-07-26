navigation
moveToLineEnd

	^ (span nextLineSeparatorIfNone: [ ^ self text endPosition ])
		startPosition