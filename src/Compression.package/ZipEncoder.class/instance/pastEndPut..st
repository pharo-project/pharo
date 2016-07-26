private
pastEndPut: anObject
	"Flush the current buffer and store the new object at the beginning"
	self commit.
	^self nextBytePut: anObject asInteger