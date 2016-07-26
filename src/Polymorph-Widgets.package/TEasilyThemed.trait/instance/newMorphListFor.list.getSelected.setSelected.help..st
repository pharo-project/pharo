controls
newMorphListFor: aModel list: listSelector getSelected: getSelector setSelected: setSelector help: helpText
	"Answer a morph list for the given model."

	^self
		newMorphListFor: aModel
		list: listSelector
		getSelected: getSelector
		setSelected: setSelector
		getEnabled: nil
		help: helpText