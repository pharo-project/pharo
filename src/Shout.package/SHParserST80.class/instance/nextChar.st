scan
nextChar
	sourcePosition := sourcePosition + 1.
	^ source
		at: sourcePosition
		ifAbsent: [Character space]