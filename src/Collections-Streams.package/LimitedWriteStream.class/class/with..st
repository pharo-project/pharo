instance creation
with: aCollection
	^ (self basicNew with: aCollection)
		setLimit: self defaultLimit
		limitBlock: nil