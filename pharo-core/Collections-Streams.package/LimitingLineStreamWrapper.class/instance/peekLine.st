accessing
peekLine

	self atEnd ifTrue: [^nil].
	^ line