testing
willStorePop
	"Answer whether the next bytecode is a store-pop."

	^ self method encoderClass isStorePopAt: pc in: self method