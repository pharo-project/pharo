scrollbars
scrollBounds
	"Return the visible scroll area taking into account whether
	the scrollbars need to be shown."
	
	|b|
	b := self innerBounds.
	self vScrollbarNeeded ifTrue: [b := b right: (b right - self scrollBarThickness)].
	self hScrollbarNeeded ifTrue: [b := b bottom: (b bottom - self scrollBarThickness)].
	^b