access options
showHScrollBarOnlyWhenNeeded: bool
	"Get rid of scroll bar for short panes that don't want it shown."

	self setProperty: #noHScrollBarPlease toValue: bool.
	self setProperty: #hScrollBarAlways toValue: bool.
	
	self hHideOrShowScrollBar.
