initialization
breakLinesAt: length
	"Configure me to break lines at lenth, a multiple of 4, and insert newlines"

	self assert: (length \\ 4) isZero description: 'line length should be a multiple of 4'.
	lineLength := length.
	lineEnd ifNil: [ self lineEndConvention: #crlf ]