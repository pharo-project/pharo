drawing
athensFillRectangle: aRect on: anAthensCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	| r |
	r := aRect.
	strokePaints
		do: [ :paint | 
			paint athensFillRectangle: r on: anAthensCanvas.
			withInset ifTrue:[r := r insetBy: paint width ]]