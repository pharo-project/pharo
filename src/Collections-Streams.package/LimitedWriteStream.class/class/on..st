instance creation
on: aCollection
	^ (self basicNew on: aCollection)
		setLimit: self defaultLimit
		limitBlock: nil