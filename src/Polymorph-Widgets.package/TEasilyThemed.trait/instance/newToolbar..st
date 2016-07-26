controls
newToolbar: controls
	"Answer a toolbar with the given controls."

	^self theme
		newToolbarIn: self
		for: controls