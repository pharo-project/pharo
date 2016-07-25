*Morphic-Base
fillWithRamp: rampSpecsOrColor oriented: aRatio 
	rampSpecsOrColor isColor
		ifTrue: [self color: rampSpecsOrColor".
			self borderColor: rampSpecsOrColor muchDarker"]
		ifFalse: [| fill | 
			fill := GradientFillStyle ramp: rampSpecsOrColor.
			fill origin: self bounds topLeft.
			fill direction: (self bounds extent * aRatio) truncated.
			fill radial: false.
			self fillStyle: fill.
			self borderColor: (rampSpecsOrColor first value mixed: 0.5 with: rampSpecsOrColor last value) muchDarker]