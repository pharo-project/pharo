morph creation
newMorphListIn: aThemedMorph for: aModel list: listSelector getSelected: getSelector setSelected: setSelector getEnabled: enabledSel help: helpText
	"Answer a morph list for the given model."

	^(PluggableMorphListMorph
			on: aModel
			list: listSelector
			selected: getSelector
			changeSelected: setSelector
			menu: nil
			keystroke: nil)
		selectionColor: self selectionColor;
		autoDeselect: false;
		cornerStyle: aThemedMorph preferredCornerStyle;
		color: Color white;
		borderStyle: (BorderStyle inset width: 1);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText
		