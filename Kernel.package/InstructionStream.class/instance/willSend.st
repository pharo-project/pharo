testing
willSend
	"Answer whether the next bytecode is a message-send."

	^ self method encoderClass isSendAt: pc in: self method