drawing
athensFillRectangle: aRect on: anAthensCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	

	paints do: [:paint | 
				paint athensFillRectangle: aRect on: anAthensCanvas ]
