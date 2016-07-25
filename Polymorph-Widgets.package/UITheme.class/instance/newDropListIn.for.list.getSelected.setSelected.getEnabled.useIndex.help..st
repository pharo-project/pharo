morph creation
newDropListIn: aThemedMorph for: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel useIndex: useIndex help: helpText
	"Answer a drop list for the given model."

	^(DropListMorph basicNew
		basicTheme: self;
			initialize;
			useSelectionIndex: useIndex;
			on: aModel
			list: listSel
			selected: getSel
			changeSelected: setSel)
		selectionColor: self selectionColor;
		getEnabledSelector: enabledSel;
		font: self dropListFont;
		listFont: self listFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText;
		basicTheme: nil