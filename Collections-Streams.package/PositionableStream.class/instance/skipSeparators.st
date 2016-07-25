public
skipSeparators
	[self atEnd]
		whileFalse:
		[self next isSeparator ifFalse: [^ self position: self position-1]]