drawing
drawOn: aCanvas
	"Draw a hatch pattern first."

	aCanvas
		fillRectangle: self innerBounds
		fillStyle: (InfiniteForm with: self hatchForm).
	super drawOn: aCanvas
