initialization
initializeWidgets
	redSlider := self instantiate: SliderInput.
	greenSlider := self instantiate: SliderInput.
	blueSlider := self instantiate: SliderInput.
	redSlider
		min: 0;
		max: 255; label: 'Red' translated.
	greenSlider
		min: 0;
		max: 255; label: 'Green' translated.
	blueSlider
		min: 0;
		max: 255; label: 'Blue' translated.
	self focusOrder
		add: redSlider;
		add: greenSlider;
		add: blueSlider