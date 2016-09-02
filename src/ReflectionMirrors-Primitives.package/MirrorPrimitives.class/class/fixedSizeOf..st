fields accessing
fixedSizeOf: anObject
	"Answer the number of inst vars"

	^(self classOf: anObject) instSize