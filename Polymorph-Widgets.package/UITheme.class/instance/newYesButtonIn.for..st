morph creation
newYesButtonIn: aThemedMorph for: aModel
	"Answer a new Yes button."

	^(self
			newButtonIn: aThemedMorph
			for: aModel
			getState: nil
			action: #yes
			arguments: nil
			getEnabled: nil
			label: 'Yes' translated
			help: 'Answer yes and close the window' translated)
		hResizing: #rigid;
		vResizing: #rigid