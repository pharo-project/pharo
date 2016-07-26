morph creation
newTreeIn: aThemedMorph for: aModel list: listSelector selected: getSelector changeSelected: setSelector
	"Answer a new tree morph."
	
	^TreeListMorph new
		selectionColor: self selectionColor;
		font: self listFont;
		on: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		menu: nil
		keystroke: nil;
		cornerStyle: aThemedMorph preferredCornerStyle;
		"color: Color white;"
		borderStyle: (BorderStyle inset width: 1);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		autoDeselect: false;
		yourself
		