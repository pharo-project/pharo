controls
newEditableDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel addToList: addSel ghostText: ghostText getEnabled: enabledSel help: helpText
	"Answer an editable drop list for the given model."

	^self theme
		newEditableDropListIn: self
		for: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		addToList: addSel
		class: String
		default: ''
		ghostText: ghostText
		getEnabled: enabledSel
		useIndex: false
		help: helpText