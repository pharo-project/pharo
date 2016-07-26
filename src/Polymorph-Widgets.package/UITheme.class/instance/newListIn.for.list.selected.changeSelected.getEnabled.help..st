morph creation
newListIn: aThemedMorph for: aModel list: listSelector selected: getSelector changeSelected: setSelector getEnabled: enabledSel help: helpText
	"Answer a list for the given model."

	^PluggableListMorph new
		selectionColor: self selectionColor;
		textColor: self listTextColor;
		font: self listFont;
		on: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		menu: nil
		keystroke: nil;
		autoDeselect: false;
		cornerStyle: aThemedMorph preferredCornerStyle;
		color: Color white;
		borderStyle: (BorderStyle inset width: 1);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText