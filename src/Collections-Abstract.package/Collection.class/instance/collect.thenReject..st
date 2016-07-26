enumerating
collect: collectBlock thenReject: selectBlock
	"Utility method to improve readability."

	^ (self collect: collectBlock) reject: selectBlock