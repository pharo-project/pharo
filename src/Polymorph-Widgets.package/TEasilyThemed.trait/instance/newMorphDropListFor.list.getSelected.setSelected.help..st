controls
newMorphDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel help: helpText
	"Answer a morph drop list for the given model."

	^self 
		newMorphDropListFor: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		getEnabled: nil
		useIndex: true
		help: helpText