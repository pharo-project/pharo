controls
newRadioButtonFor: aModel getSelected: getSel setSelected: setSel getEnabled: enabledSel label: stringOrText help: helpText
	"Answer a checkbox (radio button appearance) with the given label."

	^self theme
		newRadioButtonIn: self
		for: aModel
		getSelected: getSel
		setSelected: setSel
		getEnabled: enabledSel
		label: stringOrText
		help: helpText