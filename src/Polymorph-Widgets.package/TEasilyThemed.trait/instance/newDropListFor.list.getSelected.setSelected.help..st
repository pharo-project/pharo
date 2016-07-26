controls
newDropListFor: aModel list: listSel getSelected: getSel setSelected: setSel help: helpText
	"Answer a drop list for the given model."

	^self
		newDropListFor: aModel
		list: listSel
		getSelected: getSel
		setSelected: setSel
		getEnabled: nil
		useIndex: true
		help: helpText