positioning
skipSeparators

	[self atEnd] whileFalse: [
		self basicNext isSeparator ifFalse: [
			^ self position: self position - 1]]

