accessing
visibleTabRange: anInterval
	"Record the range of tabs that are visible.
	Also used when scrolling to determine starting index."
	
	visibleTabRange := anInterval.
	self
		changed: #leftButtonEnabled;
		changed: #rightButtonEnabled