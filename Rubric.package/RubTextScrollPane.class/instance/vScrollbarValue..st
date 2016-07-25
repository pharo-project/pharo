scrollbar managing
vScrollbarValue: scrollValue
	self announcer announce: (RubVerticalScrolled scrollValue: scrollValue).
	self handleScrollerOffsetChange: [ super vScrollbarValue: scrollValue ]