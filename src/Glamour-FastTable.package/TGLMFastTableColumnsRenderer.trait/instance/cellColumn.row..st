rendering
cellColumn: aColumn row: aRowIndex
	| item cell |
	item := self elementAt: aRowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		addMorph: (self 
			rowMorphForColumn: aColumn 
			item: item 
			withIndex: aRowIndex);
		yourself.
	^ cell