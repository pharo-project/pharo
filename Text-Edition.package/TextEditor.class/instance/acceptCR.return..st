typing support
acceptCR: aKeyboardEvent return: return
	((aKeyboardEvent keyValue = Character cr asciiValue) and: [morph acceptOnCR])
		ifTrue: [
			self closeTypeIn.
			return value: true].