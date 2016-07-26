accessing
headerColumn: column
	column id ifNil: [ ^ nil ]. 
	^ FTCellMorph new 
		listCentering: #center;
		addMorph: column id asMorph;
		yourself