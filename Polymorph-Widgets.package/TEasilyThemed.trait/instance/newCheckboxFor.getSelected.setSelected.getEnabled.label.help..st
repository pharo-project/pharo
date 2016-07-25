controls
newCheckboxFor: aModel getSelected: getSel setSelected: setSel getEnabled: enabledSel label: stringOrText help: helpText
	"Answer a checkbox with the given label."

	^self theme
		newCheckboxIn: self
		for: aModel
		getSelected: getSel
		setSelected: setSel
		getEnabled: enabledSel
		label: stringOrText
		help: helpText