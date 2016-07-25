*Morphic-Base
useGradientFill
	"Make receiver use a solid fill style (e.g., a simple color)"
	| fill color1 color2 |
	self fillStyle isGradientFill ifTrue:[^self]. "Already done"
	color1 := self color asColor.
	color2 := color1 negated.
	fill := GradientFillStyle ramp: {0.0 -> color1. 1.0 -> color2}.
	fill origin: self topLeft.
	fill direction: 0 @ self bounds extent y.
	fill normal: self bounds extent x @ 0.
	fill radial: false.
	self fillStyle: fill