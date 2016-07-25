accessing
truncateTo: aSize
	bytes size = aSize
		ifFalse: [bytes := bytes size < aSize
			ifTrue: [(ByteArray new: aSize)
				replaceFrom: 1 to: bytes size with: bytes startingAt: 1]
			ifFalse: [bytes copyFrom: 1 to: aSize]].
	size := bytes size.
	self modified.