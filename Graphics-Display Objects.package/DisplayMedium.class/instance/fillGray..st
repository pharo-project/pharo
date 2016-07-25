coloring
fillGray: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to the gray mask."

	self fill: aRectangle rule: Form over fillColor: Color gray