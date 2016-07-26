*Glamour-Morphic-Pager
renderPager: aBrowser 
	^ aBrowser hasFixedSizePanes 
		ifTrue: [GLMMorphicPagerRenderer render: aBrowser from: self]
		ifFalse: []