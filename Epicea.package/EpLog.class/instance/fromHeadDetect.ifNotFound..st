enumerating
fromHeadDetect: aBlockReturningBoolean ifNotFound: notFoundBlock

	^ self
		from: self headReference
		detect: aBlockReturningBoolean
		ifNotFound: notFoundBlock