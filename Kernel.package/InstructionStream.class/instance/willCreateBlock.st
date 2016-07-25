testing
willCreateBlock
	"next bytecode is a block creation"

	^ self method encoderClass isCreateBlockAt: pc in: self method