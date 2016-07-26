converting
asAthensPaintOn: aCanvas
	^ aCanvas surface
			createLinearGradient: self colorRamp
				start: self start 
				stop: self stop