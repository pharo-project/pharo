scan
peekChar
	^ source
		at: sourcePosition + 1
		ifAbsent: [Character space]