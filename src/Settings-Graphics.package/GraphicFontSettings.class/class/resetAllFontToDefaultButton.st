fonts
resetAllFontToDefaultButton
	^ (self theme
		newButtonIn: World
		for: self
		getState: nil
		action: #resetAllFontToDefault
		arguments: {}
		getEnabled: nil
		getLabel: nil
		help: 'Force all system fonts to be the default one' translated)
		label:
			(self theme
				newRowIn: World
				for:
					{(AlphaImageMorph new
						image: (self iconNamed: #smallRedoIcon)).
					(self theme buttonLabelForText: 'Force all' translated)});
		yourself