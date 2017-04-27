enumerating
fromHeadDetect: aBlockReturningBoolean

	^ self
		from: self headReference
		detect: aBlockReturningBoolean