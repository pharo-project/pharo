view
defaultTreeMorph
	|  col |
	col := MorphTreeColumn new 
		rowMorphGetSelector: [:node | StringMorph contents: node item asString].
	
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
			itemStringGetter: [:item | self wrapItem: item];
			isCheckList: self isCheckList";
			rowColorForEven: Color veryLightGray muchLighter odd: Color white".