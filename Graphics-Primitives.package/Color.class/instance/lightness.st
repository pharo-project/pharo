accessing
lightness
	"Return the HSL lightness of this color, a float in the range [0.0..1.0]."

	^ (self privateMaxComponent + self privateMinComponent) asFloat
		/ ComponentMax / 2.0