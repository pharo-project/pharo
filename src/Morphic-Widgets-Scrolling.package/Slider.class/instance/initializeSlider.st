initialization
initializeSlider
	slider := BorderedMorph newBounds: self totalSliderArea color: self thumbColor.
	sliderShadow := BorderedMorph newBounds: self totalSliderArea
						color: self pagingArea color.
	slider on: #mouseMove send: #scrollAbsolute: to: self.
	slider on: #mouseDown send: #mouseDownInSlider: to: self.
	slider on: #mouseUp send: #mouseUpInSlider: to: self.
	slider 
		borderWidth: 1;
		borderColor: Color lightGray.

	sliderShadow 
		borderWidth: 1;
		borderColor: #inset.
	"(the shadow must have the pagingArea as its owner to highlight properly)"
	self pagingArea addMorph: sliderShadow.
	sliderShadow hide.
	self addMorph: slider.
	self computeSlider.
