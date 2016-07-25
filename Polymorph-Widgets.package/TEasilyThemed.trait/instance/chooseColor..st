services
chooseColor: aColor 
	"Answer the result of a color selector dialog with the given color."

	^self theme
		chooseColorIn: self
		title: 'Colour Selector' translated
		color: aColor