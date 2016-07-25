accessing
luminance
	"Return the luminance of this color, a brightness value weighted by the human eye's color sensitivity."

	^ ((299 * self privateRed) +
	   (587 * self privateGreen) +
	   (114 * self privateBlue)) / (1000 * ComponentMax)
