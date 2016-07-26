other events
mouseUpInSlider: event 

	slider borderStyle style == #inset
		ifTrue: [slider borderColor: #raised].
	
	sliderShadow hide