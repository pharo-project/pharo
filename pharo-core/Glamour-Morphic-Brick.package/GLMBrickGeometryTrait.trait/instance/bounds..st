brick-geometry
bounds: aRectangle
	"when you use this method, it changes brickBounds
	then recalculates layout and invalidates rectangle"
	self layoutAfter: [ :b | b wrappedBounds bounds: aRectangle ]