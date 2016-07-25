scrollbar managing
hScrollbarValue: scrollValue
	self announcer announce: (RubHorizontalScrolled scrollValue: scrollValue).
	self handleScrollerOffsetChange: [ super hScrollbarValue: scrollValue ]