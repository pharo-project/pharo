services
chooseColor: aColor title: title
	"Answer the result of a color selector dialog with the given title and initial colour."

	^self theme
		chooseColorIn: self
		title: title
		color: aColor