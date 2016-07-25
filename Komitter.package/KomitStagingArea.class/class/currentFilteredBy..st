instance creation
currentFilteredBy: aFilterBlock

	^ super new
		filterBlock: aFilterBlock;
		yourself