row management
hUnadjustedScrollRange
"Ok, this is a bit messed up. We need to return the width of the widest item in the list. If we grab every item in the list, it defeats the purpose of LazyListMorph. If we don't, then we don't know the size. 

This is a compromise -- find the widest of the first 30 items, then double it, This width will be updated as new items are installed, so it will always be correct for the visible items. If you know a better way, please chime in."

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
		maxWidth := maxWidth max: (item widthToDisplayInList: self).
		index:= index + 1.
		].

	"Add some initial fudge if we didn't check all the items."
	(itemsToCheck < listItems size) ifTrue:[maxWidth := maxWidth*2].

	^maxWidth
