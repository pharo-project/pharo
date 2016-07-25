controls
newStack: controls
	"Answer a morph laid out with a stack of controls."

	^self theme
		newStackIn: self
		for: controls