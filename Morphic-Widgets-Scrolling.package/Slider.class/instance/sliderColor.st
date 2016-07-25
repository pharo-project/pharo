access
sliderColor
	"color scheme for the whole slider widget"
	sliderColor ifNil: [^ (color alphaMixed: 0.7 with: Color white) slightlyLighter].
	^ sliderColor