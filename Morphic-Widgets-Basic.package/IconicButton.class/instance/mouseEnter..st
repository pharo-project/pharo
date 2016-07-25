mouse events
mouseEnter: evt
	"Remember the old border style."
	
	oldBorderStyle:= self borderStyle.
	self borderStyle: self mouseOverBorderStyle