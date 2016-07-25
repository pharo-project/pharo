scrollbars
hResizeScrollbar
	"Resize the horizontal scrollbar to fit the receiver."
	
	|b|
	b := self innerBounds.
	b := b top: b bottom - self scrollBarThickness.
	self vScrollbarShowing ifTrue: [
		b := b right: b right - self scrollBarThickness].
	self hScrollbar bounds: b