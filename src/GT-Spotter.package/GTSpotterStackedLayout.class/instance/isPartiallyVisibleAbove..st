testing
isPartiallyVisibleAbove: aBrick

	^ (self isFullyVisible: aBrick) not & (self isAbove: aBrick)