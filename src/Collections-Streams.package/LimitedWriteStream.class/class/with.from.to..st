instance creation
with: aCollection from: firstIndex to: lastIndex
	^ (self basicNew with: aCollection from: firstIndex to: lastIndex)
		setLimit: self defaultLimit
		limitBlock: nil