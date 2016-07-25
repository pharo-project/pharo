controls
newHSVASelector: aColor help: helpText
	"Answer a hue-saturation-volume selector with the given color."

	^self theme
		newHSVASelectorIn: self
		color: aColor
		help: helpText