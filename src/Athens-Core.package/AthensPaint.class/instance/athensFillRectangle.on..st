rendering
athensFillRectangle: aRectangle on: aCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	
	self subclassResponsibility