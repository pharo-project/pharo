controls
newColumn: controls
	"Answer a morph laid out with a column of controls."

	^self theme
		newColumnIn: self
		for: controls