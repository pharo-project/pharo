drawing
shadowForm
	"Return a form representing the 'shadow' of the receiver - e.g., all pixels that are occupied by the receiver are one, all others are zero."
	| canvas |
	canvas := (Display defaultCanvasClass extent: self fullBounds extent depth: 1)
				asShadowDrawingCanvas: Color black. "Color black represents one for 1bpp"
	canvas translateBy: bounds topLeft negated
		during:[:tempCanvas| tempCanvas fullDrawMorph: self].
	^ canvas form offset: bounds topLeft
