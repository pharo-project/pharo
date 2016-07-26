accessing
cellColumn: column row: rowIndex
	| element cell |
	element := self elementAt: rowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		addMorph: (self rowMorphForElement: element);
		yourself.
	^ cell