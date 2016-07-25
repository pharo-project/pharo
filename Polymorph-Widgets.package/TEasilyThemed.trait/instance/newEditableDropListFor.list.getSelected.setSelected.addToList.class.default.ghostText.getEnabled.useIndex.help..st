controls
newEditableDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel addToList: addSel class: aClass default: defaultValue ghostText: ghostText getEnabled: enabledSel useIndex: useIndex help: helpText
	"Answer an editable drop list for the given model."

	^self theme
		newEditableDropListIn: self
		for: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		addToList: addSel
		class: aClass
		default: defaultValue
		ghostText: ghostText
		getEnabled: enabledSel
		useIndex: useIndex
		help: helpText