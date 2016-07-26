controls
newMorphDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel useIndex: useIndex help: helpText
	"Answer a morph drop list for the given model."

	^self theme
		newMorphDropListIn: self
		for: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		getEnabled: enabledSel
		useIndex: useIndex
		help: helpText