rendering
athensFillRectangle: aRect on: canvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	

	^ canvas surface fillRectangle: aRect color: color