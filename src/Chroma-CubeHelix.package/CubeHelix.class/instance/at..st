accessing
at: gradientParameter
	| gradientGamma phi alpha |
	gradientGamma := gradientParameter raisedTo: gamma.
	phi := 2.0 * Float pi * (hueShift / 3.0 + (rotations * gradientParameter)).
	alpha := saturation * gradientGamma * (1.0 - gradientGamma) / 2.0.

	^ Color fromRgbTriplet: (
		gradientGamma + (alpha * Coefficients +* (Array with: phi cos with: phi sin))
			collect: [ :c | c min: 1 max: 0 ])