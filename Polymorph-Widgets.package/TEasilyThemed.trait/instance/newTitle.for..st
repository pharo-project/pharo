controls
newTitle: aString for: control
	"Answer a morph laid out with a column with a title."

	^self theme
		newTitleIn: self
		label: aString
		for: control