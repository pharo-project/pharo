controls
newAlphaImage: aForm help: helpText
	"Answer an alpha image morph."

	^self theme
		newAlphaImageIn: self
		image: aForm
		help: helpText