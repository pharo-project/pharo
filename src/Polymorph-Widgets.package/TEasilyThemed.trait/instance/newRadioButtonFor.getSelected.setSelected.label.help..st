controls
newRadioButtonFor: aModel getSelected: getSel setSelected: setSel label: stringOrText help: helpText
	"Answer a checkbox (radio button appearance) with the given label."

	^self
		newRadioButtonFor: aModel
		getSelected: getSel
		setSelected: setSel
		getEnabled: nil
		label: stringOrText
		help: helpText