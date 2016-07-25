pager-actions
showOrHideScrollBar
	
	((self pagerModel size > self pagerModel minimumPageNumberForScrollbar) & (self showScrollBarWhenNeeded))
		ifTrue: [ (self hasSubbrick: self pagerBar)
			ifFalse: [ self addBrickBack: self pagerBar ] ]
		ifFalse: [ self removeBrick: self pagerBar ].