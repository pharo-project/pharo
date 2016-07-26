conversions
setAlpha: aFloat
	alpha := ((255.0 * aFloat) asInteger  min: 255) max: 0.
