enumerating
reject: rejectBlock thenCollect: collectBlock
	"Utility method to improve readability."

	^ (self reject: rejectBlock) collect: collectBlock