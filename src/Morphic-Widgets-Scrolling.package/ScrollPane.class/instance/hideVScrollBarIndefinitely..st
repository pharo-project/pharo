access options
hideVScrollBarIndefinitely: bool
	"Get rid of scroll bar for short panes that don't want it shown."

	self setProperty: #noVScrollBarPlease toValue: bool.
	self vHideOrShowScrollBar.
