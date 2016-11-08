examples
wheel: thisMany
	"Return a collection of thisMany colors evenly spaced around the color wheel."
	"(Color wheel: 12) inspect"

	^ self wheel: thisMany saturation: 0.9 brightness: 0.7
