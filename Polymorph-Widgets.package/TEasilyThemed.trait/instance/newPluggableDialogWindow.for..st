controls
newPluggableDialogWindow: title for: contentMorph
	"Answer a new pluggable dialog with the given content."

	^self theme
		newPluggableDialogWindowIn: self
		title: title
		for: contentMorph