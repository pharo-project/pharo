accessing
findText: aStringOrText isRegex: isReg caseSensitive: isCaseSensitive entireWordsOnly: forEntireWordsOnly
		self isRegex: isReg.
		self findText: aStringOrText.
		self entireWordsOnly: forEntireWordsOnly.
		self caseSensitive: isCaseSensitive