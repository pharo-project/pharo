morph creation
newMorphDropListIn: aThemedMorph for: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel useIndex: useIndex help: helpText
	"Answer a morph drop list for the given model."

	^(MorphDropListMorph
			on: aModel
			list: listSel
			selected: getSel
			changeSelected: setSel
			useIndex: useIndex)
		selectionColor: self selectionColor;
		getEnabledSelector: enabledSel;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText