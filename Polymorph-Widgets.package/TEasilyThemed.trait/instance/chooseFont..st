services
chooseFont: aFont 
	"Answer the result of a font selector dialog with the given initial font."

	^self theme
		chooseFontIn: self
		title: 'Font Selector' translated
		font: aFont