accessing
chroma
	"Return the chroma of this color, a float in the range [0.0..1.0].
	Some combinations of chroma and brightness (or lightness) are meaningless in RGB; therefore, HSL and HSV each define a specific saturation by scaling chroma to the full [0..1] range."

	^ (self privateMaxComponent - self privateMinComponent) asFloat / ComponentMax