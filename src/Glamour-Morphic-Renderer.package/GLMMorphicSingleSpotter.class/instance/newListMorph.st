morphic
newListMorph
	listMorph := PluggableListMorph
		on: self
		list: #listLabels
		selected: #listIndex
		changeSelected: #listIndex:.
	listMorph
		borderStyle: (self theme listNormalBorderStyleFor: listMorph);
		color: (self theme listNormalFillStyleFor: listMorph);
		cornerStyle: self preferredCornerStyle;
		doubleClickSelector: #doubleClick;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		autoDeselect: false.
	^ listMorph