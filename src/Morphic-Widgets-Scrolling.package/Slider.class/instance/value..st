model access
value: newValue
	"Drive the slider position externally..."
	value := newValue min: 1.0 max: 0.0.
	self computeSlider