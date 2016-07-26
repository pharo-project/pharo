rectangle functions
allAreasOutsideList: aCollection do: aBlock
	"Enumerate aBlock with all areas of the receiver not overlapping 
	any rectangle in the given collection"
	^self allAreasOutsideList: aCollection startingAt: 1 do: aBlock