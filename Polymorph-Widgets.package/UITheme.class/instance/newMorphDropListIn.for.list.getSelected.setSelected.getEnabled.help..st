morph creation
newMorphDropListIn: aThemedMorph for: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel help: helpText
	"Answer a morph drop list for the given model."

	^self
		newMorphDropListIn: aThemedMorph
		for: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		getEnabled: enabledSel
		useIndex: true
		help: helpText