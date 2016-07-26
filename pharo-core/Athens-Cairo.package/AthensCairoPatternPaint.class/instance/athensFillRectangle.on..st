drawing
athensFillRectangle: aRectangle on: aCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	aCanvas
		rectangleX: aRectangle left
		y: aRectangle top
		width: aRectangle width
		height: aRectangle height.
	
	self setMatrix: aCanvas paintTransform inverted.
	self loadOnCairoCanvas: aCanvas.
	aCanvas fill