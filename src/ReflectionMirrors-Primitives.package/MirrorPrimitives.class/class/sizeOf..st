fields accessing
sizeOf: anObject
	"Answer the number of inst vars + number of indexible fields"

	^(self fixedSizeOf: anObject) + (self indexableSizeOf: anObject)