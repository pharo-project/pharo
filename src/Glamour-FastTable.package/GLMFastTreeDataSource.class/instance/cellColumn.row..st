accessing
cellColumn: aColumnIndex row: aRowIndex
	| item cell |
	item := self elementAt: aRowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		addMorph: (self rowMorphForItem: item);
		yourself.
	^ cell