accessing
cellColumn: column row: rowIndex 
	^ FTCellMorph new
		addMorphBack: (self elementAt: rowIndex) asMorph;
		yourself
	
	