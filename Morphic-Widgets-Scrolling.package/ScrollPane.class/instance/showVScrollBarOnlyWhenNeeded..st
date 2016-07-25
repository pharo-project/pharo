access options
showVScrollBarOnlyWhenNeeded: bool
	"Get rid of scroll bar for short panes that don't want it shown."

	self setProperty: #noVScrollBarPlease toValue: bool.
	self setProperty: #vScrollBarAlways toValue: bool.
	self vHideOrShowScrollBar.
