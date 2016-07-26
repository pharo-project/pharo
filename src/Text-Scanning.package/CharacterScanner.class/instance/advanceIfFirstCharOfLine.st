private
advanceIfFirstCharOfLine
	lastIndex = line first
		ifTrue:
			[destX := destX + pendingKernX + (font widthOf: (text at: line first)).
			lastIndex := lastIndex + 1.
			pendingKernX := 0].