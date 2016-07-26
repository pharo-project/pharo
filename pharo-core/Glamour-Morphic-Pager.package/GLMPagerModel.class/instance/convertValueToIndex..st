scrolling
convertValueToIndex: aValue
	
	^ (self size - self actualNumberOfVisiblePages) * aValue + 1.