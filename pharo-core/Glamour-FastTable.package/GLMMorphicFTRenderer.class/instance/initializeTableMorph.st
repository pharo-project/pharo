initialization
initializeTableMorph
	| presentation |
	presentation := tableModel glamourPresentation.
	tableMorph := self createFastTable.
	tableMorph
		dataSource: tableModel;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cornerStyle: tableMorph preferredCornerStyle;
		borderStyle: (BorderStyle inset width: 0);
		dragEnabled: presentation allowsItemDrag;
		dropEnabled: presentation isDropTarget;
		allowsDeselection: presentation allowsDeselection.
	self setFastTableLayoutFrame: tableMorph.
	presentation isMultiple
		ifTrue: [ tableMorph beMultipleSelection ]
		ifFalse: [ tableMorph beSingleSelection ].
	self specificTableMorphInitializiation