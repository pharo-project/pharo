items creation
defaultTreeMorph
	|  col |
	col := MorphTreeColumn new
		rowMorphGetSelector: [:node | node displayString asMorph ].
	
	self headerLabel
		ifNotNil: [	col headerButtonLabel: self headerLabel font: nil].
			
	^ (self treeMorphClass on: self) 
			columns:  (Array with: col);
			hResizing: #spaceFill;
			vResizing: #spaceFill;
			resizerWidth: 0;
			columnInset: 0; 
			rowInset: 2; 
			keystrokeActionSelector: #keyStroke:;
			"preferedPaneColor: Color white;"
			multiSelection: self multiSelection;
			autoMultiSelection: self autoMultiSelection;
			isCheckList: self isCheckList;
			doubleClickSelector: #doubleClick;
			getMenuSelector: #menu:shifted:";
			rowColorForEven: Color veryLightGray muchLighter odd: Color white".