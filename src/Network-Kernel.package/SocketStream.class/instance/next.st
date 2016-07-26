stream in
next
	"Return next byte, if inBuffer is empty
	we recieve some more data and try again."

	self atEnd ifTrue: [^nil].
	self isInBufferEmpty ifTrue:
		[self receiveData.
		self atEnd ifTrue: [^nil]].
	lastRead := lastRead + 1.
	^inBuffer at: lastRead