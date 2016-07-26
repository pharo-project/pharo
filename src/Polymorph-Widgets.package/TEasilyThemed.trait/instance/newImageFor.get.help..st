controls
newImageFor: aModel get: getSel help: helpText
	"Answer a text entry for the given model."

	^self theme
		newImageIn: self
		for: aModel
		get: getSel
		help: helpText