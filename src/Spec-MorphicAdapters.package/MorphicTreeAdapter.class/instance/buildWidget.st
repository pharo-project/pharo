factory
buildWidget
	^ MorphTreeMorph new
		model: self treeModel;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		initializeColumsFrom: self;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		autoDeselection: self autoDeselection;
		resizerWidth: self resizerWidth;
		columnInset: self columnInset;
		iconBlock: self iconBlock;
		rowInset: self rowInset;
		keystrokeActionSelector: #keyStroke:;
		multiSelection: self multiSelection;
		autoMultiSelection: self autoMultiSelection;
		removeOnlyLastSelected: self selectOnlyLastHighlighted;
		isCheckList: self isCheckList;
		doubleClickSelector: #doubleClick;
		getMenuSelector: #menu:shifted:;
		rowColorForEven: self evenRowColor;
		rowColorForOdd: self oddRowColor;
		buildContents;
		yourself