testing
willReturn
	"Answer whether the next bytecode is a return."

	^ self method encoderClass isReturnAt: pc in: self method