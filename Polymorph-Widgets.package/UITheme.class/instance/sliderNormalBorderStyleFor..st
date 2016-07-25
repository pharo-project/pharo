border-styles
sliderNormalBorderStyleFor: aSlider
	"Return the normal slider borderStyle for the given text editor."

	^BorderStyle inset
		width: 1;
		baseColor: aSlider paneColor twiceDarker