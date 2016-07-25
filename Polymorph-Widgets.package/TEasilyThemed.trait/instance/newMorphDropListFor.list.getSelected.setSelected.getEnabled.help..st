controls
newMorphDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel help: helpText
	"Answer a morph drop list for the given model."

	^self 
		newMorphDropListFor: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		getEnabled: enabledSel
		useIndex: true
		help: helpText