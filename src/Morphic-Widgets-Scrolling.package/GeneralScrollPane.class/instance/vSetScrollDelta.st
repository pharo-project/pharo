geometry
vSetScrollDelta
	"Set the vertical scrollbar delta, value and interval, based on the current scroll bounds and offset."
	
	|pd|
	pd := self vPageDelta.
	self vScrollbar
		scrollDelta: pd / 10 
		pageDelta: pd;
		interval: self vScrollbarInterval;
		setValue: self vScrollbarValue