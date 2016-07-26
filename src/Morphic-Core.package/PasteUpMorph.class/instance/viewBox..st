project state
viewBox: newViewBox 
	"I am now displayed within newViewBox; react."

	super position: newViewBox topLeft.
	fullBounds := bounds := newViewBox.

	