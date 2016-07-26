access
sliderColor: newColor
	"Set the slider colour."
	
	super sliderColor: newColor.
	slider ifNotNil: [slider borderStyle baseColor: newColor]