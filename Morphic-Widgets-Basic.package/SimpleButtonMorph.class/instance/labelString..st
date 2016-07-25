accessing
labelString: aString

	label ifNil: [self label: aString]
		ifNotNil:
			[label contents: aString.
			self fitContents]
