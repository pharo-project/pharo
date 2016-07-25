initialize-release
initializeRadialBetween: origin
			extending: innerRadius
			and: outerOrigin
			extending: outerRadius
			withColorRamp: aRamp
			
	self initialize.		
	aRamp do:[:each|
		self
			addColorStopAt: each key
			colored: each value ]
	