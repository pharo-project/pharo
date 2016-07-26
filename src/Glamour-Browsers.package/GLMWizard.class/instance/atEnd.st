testing
atEnd

	^ self currentIndex = self steps size and: [
		self stepToUse atEnd
		]