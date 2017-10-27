instruction stream support
isRealSendAt: pc in: method
	"Answer whether the bytecode at pc is a real message-send, not blockCopy:."

	^self isSendAt: pc in: method