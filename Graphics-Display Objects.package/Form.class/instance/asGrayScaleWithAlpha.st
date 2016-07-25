converting
asGrayScaleWithAlpha
	"Unlike asGrayScale, this method fully preserves the alpha channel and only desaturates the form(makes grayscale)."
	^ (self asFormOfDepth: 32) collectColors: [ :c |  |l|
		l := c luminance.
		Color r:l g: l b: l alpha: c alpha ]