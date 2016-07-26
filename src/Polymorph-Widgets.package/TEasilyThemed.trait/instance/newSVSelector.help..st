controls
newSVSelector: aColor help: helpText
	"Answer a saturation-volume selector with the given color."

	^self theme
		newSVSelectorIn: self
		color: aColor
		help: helpText