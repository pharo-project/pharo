rendering
athensFillPath: aPath on: canvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	
	| fill |
	fill := self convertGradientToPaintOn: canvas.
	fill athensFillPath: aPath on: canvas.
