enumerating
collect: collectBlock thenSelect: selectBlock
	"Utility method to improve readability."

	^ (self collect: collectBlock) select: selectBlock