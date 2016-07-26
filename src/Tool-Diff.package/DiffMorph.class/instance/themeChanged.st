updating
themeChanged
	"Update the scrollbar width/frame."

	|offset|
	super themeChanged.
	self scrollbarMorph width: self theme scrollbarThickness.
	offset := self scrollbarMorph width negated - self mapMorph width.
	self scrollbarMorph layoutFrame leftOffset: offset.
	self dstMorph layoutFrame rightOffset: offset