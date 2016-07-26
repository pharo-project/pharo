coloring
reverse: aRectangle
	"Change all the bits in the receiver's area that intersects with aRectangle 
	that are white to black, and the ones that are black to white."

	self fill: aRectangle rule: Form reverse fillColor: (self quickHighLight: self depth)