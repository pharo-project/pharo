other events
mouseDownInSlider: event 

	slider borderStyle style == #raised
		ifTrue: [slider borderColor: #inset].
	
	sliderShadow color: self sliderShadowColor.
	sliderShadow cornerStyle: slider cornerStyle.
	sliderShadow bounds: slider bounds.
	sliderShadow show