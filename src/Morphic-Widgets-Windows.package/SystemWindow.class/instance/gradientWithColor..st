initialization
gradientWithColor: aColor

	| ramp |
	ramp := {0.0 -> Color white. 1.0 -> aColor}.

	^ (GradientFillStyle ramp: ramp)
		radial: true;
		origin: self bounds origin;
		direction: 0 @ 223;
		normal: 223 @ 0.