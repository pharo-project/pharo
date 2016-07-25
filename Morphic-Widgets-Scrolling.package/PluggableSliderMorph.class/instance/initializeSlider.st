initialization
initializeSlider
	"Make the slider raised."
	
	super initializeSlider.
	slider borderStyle: (BorderStyle raised baseColor: slider color; width: 1)