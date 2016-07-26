private
marginTabAt: marginIndex side: sideIndex 
	"The marginTabsArray is an Array of tuples.  The Array is indexed
	according to the marginIndex, the 'nesting' level of the requestor.
	sideIndex is 1 for left, 2 for right."

	(marginIndex > 0 and: [marginIndex < marginTabsArray size])
		ifTrue: [^(marginTabsArray at: marginIndex) at: sideIndex]
		ifFalse: [^0]