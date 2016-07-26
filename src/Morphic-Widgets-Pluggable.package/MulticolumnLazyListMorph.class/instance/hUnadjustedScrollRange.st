scroll range
hUnadjustedScrollRange
"bvr - Introduce here the old version of the super method just waiting for this class to be deleted"

	| itemsToCheck item index |
	"Check for a cached value"
	maxWidth ifNotNil:[^maxWidth].

	listItems isEmpty ifTrue: [^0]. "don't set maxWidth if empty do will be recomputed when there are some items"
	"Compute from scratch"
	itemsToCheck := 30 min: (listItems size).
	maxWidth := 0. 

	"Check the first few items to get a representative sample of the rest of the list."
	index := 1.
	[index < itemsToCheck] whileTrue:
		[ item := self getListItem: index. "Be careful not to actually install this item"
		maxWidth := maxWidth max: (self widthToDisplayItem: item).
		index:= index + 1.
		].

	"Add some initial fudge if we didn't check all the items."
	(itemsToCheck < listItems size) ifTrue:[maxWidth := maxWidth*2].

	^maxWidth + 150
