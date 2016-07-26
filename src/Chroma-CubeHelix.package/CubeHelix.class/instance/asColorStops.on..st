conversion - athens
asColorStops: stopsNumber on: aGradientPaint
	0.0 to: 1.0 by: (stopsNumber - 1) reciprocal do: [ :offset |
		aGradientPaint
			atStop: offset
			put: (self at: offset) ].
	^ aGradientPaint