accessing
cellColumn: column row: rowIndex 
	^ FTCellMorph new
		cellInset: 5;
		in: [ :me | 
			(self iconFor: rowIndex) 
				ifNotNil: [ :icon | me addMorphBack: icon asMorph ] ];
		addMorphBack: (self displayFor: rowIndex) asMorph;
		yourself