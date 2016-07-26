stream in
peek
	"Return next byte, if inBuffer is empty
	we recieve some more data and try again.
	Do not consume the byte."

	self atEnd ifTrue: [^nil].
	self isInBufferEmpty ifTrue:
		[self receiveData.
		self atEnd ifTrue: [^nil]].
	^inBuffer at: lastRead+1