scrollbars
vResizeScrollbar
	"Resize the vertical scrollbar to fit the receiver."
	
	|b|
	b := self innerBounds.
	b := b left: b right - self scrollBarThickness.
	self hScrollbarShowing ifTrue: [
		b := b bottom: b bottom - self scrollBarThickness].
	self vScrollbar bounds: b