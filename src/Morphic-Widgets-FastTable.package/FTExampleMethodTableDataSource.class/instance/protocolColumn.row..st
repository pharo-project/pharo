accessing
protocolColumn: column row: rowIndex
	^ FTCellMorph new
		color: (self colorFor: rowIndex);
		addMorph: (self elementAt: rowIndex) protocol asStringMorph;
		yourself