coloring
fillWhite: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to white."

	self fill: aRectangle rule: Form over fillColor: Color white.
