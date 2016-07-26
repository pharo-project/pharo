initialization
initialize
	super initialize.
	sliderPosition := 0.
	thickness  := 0.1.
	captured := false.
	
	self computeSliderBounds 