examples
wheel: thisMany saturation: s brightness: v
	"Return a collection of thisMany colors evenly spaced around the color wheel, all of the given saturation and brightness."
	"(Color wheel: 12 saturation: 0.4 brightness: 1.0) inspect"
	"(Color wheel: 12 saturation: 0.8 brightness: 0.5) inspect"

	^ (self h: 0.0 s: s v: v) wheel: thisMany
