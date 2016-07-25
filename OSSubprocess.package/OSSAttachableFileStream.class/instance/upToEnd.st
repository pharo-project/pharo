read, write, position
upToEnd
	"Answer a subcollection from the current access position through the last element
	of the receiver. This is slower than the method in StandardFileStream, but it
	works with pipes which answer false to #atEnd when no further input is
	currently available, but the pipe is not yet closed."

	| newStream buffer nextBytes |
	buffer := buffer1 species new: 1000.
	newStream := WriteStream on: (buffer1 species new: 100).
	[self atEnd or: [(nextBytes := self nextInto: buffer) isEmpty]]
		whileFalse: [newStream nextPutAll: nextBytes].
	^ newStream contents
