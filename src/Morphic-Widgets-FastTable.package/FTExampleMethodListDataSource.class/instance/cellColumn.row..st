accessing
cellColumn: column row: rowIndex 
	^ FTCellMorph new
		cellInset: 5;
		addMorphBack: (self iconFor: rowIndex) asMorph;
		addMorphBack: (self elementAt: rowIndex) name asMorph;
		yourself