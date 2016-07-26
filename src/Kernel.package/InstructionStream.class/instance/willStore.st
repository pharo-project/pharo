testing
willStore
	"Answer whether the next bytecode is a store or store-pop"

	^ self method encoderClass isStoreAt: pc in: self method