*Morphic-Base
externalName
	"Answer an external name by which the receiver is known. Generic
	implementation here is a transitional backstop. probably"
	^ [(self asString copyWithout: Character cr)
		truncateTo: 27]
		ifError: [ ^ self class name printString] 