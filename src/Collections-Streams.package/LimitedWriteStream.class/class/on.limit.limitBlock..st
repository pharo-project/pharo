instance creation
on: aCollection limit: anInteger limitBlock: aLimitBlock
	^ (self basicNew on: aCollection)
		setLimit: anInteger
		limitBlock: aLimitBlock