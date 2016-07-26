buttons-testing
isScrollMustBeChecked: anIndex withLeft: leftBound andRight: rightBound
	| buttonCenter |
	
	buttonCenter := self buttonCenterByIndex: anIndex.
	^ (leftBound <= buttonCenter ) & (rightBound > buttonCenter)