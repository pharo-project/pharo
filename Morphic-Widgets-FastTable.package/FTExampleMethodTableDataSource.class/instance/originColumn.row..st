accessing
originColumn: column row: rowIndex
	^ FTCellMorph new 
		color: (self colorFor: rowIndex);
		addMorph: (self elementAt: rowIndex) origin name asMorph;
		yourself