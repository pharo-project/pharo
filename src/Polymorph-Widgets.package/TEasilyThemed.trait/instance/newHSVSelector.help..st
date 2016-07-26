controls
newHSVSelector: aColor help: helpText
	"Answer a hue-saturation-volume selector with the given color."

	^self theme
		newHSVSelectorIn: self
		color: aColor
		help: helpText