fill-styles
sliderDisabledFillStyleFor: aSlider
	"Return the disabled fillStyle for the given slider."
	
	^aSlider paneColor alphaMixed: 0.3 with: Color white