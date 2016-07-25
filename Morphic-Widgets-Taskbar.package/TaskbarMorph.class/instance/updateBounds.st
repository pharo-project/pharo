private - layout
updateBounds
	"Update the receiver's bounds to fill the world."

	self
		width: self owner width;
		snapToEdgeIfAppropriate