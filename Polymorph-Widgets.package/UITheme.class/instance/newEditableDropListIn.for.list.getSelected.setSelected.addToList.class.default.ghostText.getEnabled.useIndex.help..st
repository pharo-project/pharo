morph creation
newEditableDropListIn: aThemedMorph for: aModel list: listSel getSelected: getSel setSelected: setSel addToList: addSel class: aClass default: defaultValue ghostText: ghostText getEnabled: enabledSel useIndex: useIndex help: helpText
	"Answer an editable drop list for the given model."

	^(EditableDropListMorph
			on: aModel
			list: listSel
			selected: getSel
			changeSelected: setSel
			useIndex: useIndex
			addToList: addSel
			class: aClass
			getEnabled: enabledSel
			default: defaultValue)
		ghostText: ghostText;
		selectionColor: self selectionColor;
		font: self dropListFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText