drawing
athensFillRectangle: aRect on: anAthensCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	topLeftPaint
		athensFillPath: (self pathForTopLeftBorderOf: (aRect expandBy: width/2.0) on: anAthensCanvas)
		on: anAthensCanvas.
	bottomRightPaint
		athensFillPath: (self pathForBottomRightBorderOf: (aRect expandBy: width/2.0) on: anAthensCanvas)
		on: anAthensCanvas