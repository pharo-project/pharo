testing
atEnd
	"Answer whether the receiver can access any more objects."

	^ writer closed and: [self peek == nil]
