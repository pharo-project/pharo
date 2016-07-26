morph creation
newNoButtonIn: aThemedMorph for: aModel
	"Answer a new No button."

	^(self
			newButtonIn: aThemedMorph
			for: aModel
			getState: nil
			action: #no
			arguments: nil
			getEnabled: nil
			label: 'No' translated
			help: 'Answer no and close the window' translated)
		hResizing: #rigid;
		vResizing: #rigid