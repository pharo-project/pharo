geometry
hSetScrollDelta
	"Set the horizontal scrollbar delta, value and interval, based on the current scroll bounds and offset."
	
	|pd|
	pd := self hPageDelta.
	self hScrollbar
		scrollDelta: pd / 10 
		pageDelta: pd;
		interval: self hScrollbarInterval;
		setValue: self hScrollbarValue