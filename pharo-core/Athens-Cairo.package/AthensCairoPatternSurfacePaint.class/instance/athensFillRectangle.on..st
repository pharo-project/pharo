drawing
athensFillRectangle: aRectangle on: aCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	extent
		ifNotNil: [ self setRect: (origin extent: extent) forFillOn: aCanvas ]
		ifNil: [ self setRect: aRectangle forFillOn: aCanvas ].
		
	self setMatrix: aCanvas paintTransform inverted.
	self loadOnCairoCanvas: aCanvas.
	aCanvas fill