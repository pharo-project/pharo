adding
updateIndex
	self size <= self maxSize
		ifFalse: [self removeFirst].
	index := self size.
