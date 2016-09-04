instance creation
on: aCollection from: firstIndex to: lastIndex
	^ (self basicNew on: aCollection from: firstIndex to: lastIndex)
		setLimit: self defaultLimit
		limitBlock: nil