controls
newMorphListFor: aModel list: listSelector getSelected: getSelector setSelected: setSelector getEnabled: enabledSel help: helpText
	"Answer a morph list for the given model."

	^self theme
		newMorphListIn: self
		for: aModel
		list: listSelector
		getSelected: getSelector
		setSelected: setSelector
		getEnabled: enabledSel
		help: helpText