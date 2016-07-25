accessing
cellColumn: column row: rowIndex 
	column id = '#' ifTrue: [ ^ FTCellMorph new addMorph: rowIndex asMorph ].
	column id = 'Name' ifTrue: [ ^ self nameColumn: column row: rowIndex ].
	column id = 'Protocol' ifTrue: [ ^ self protocolColumn: column row: rowIndex ].
	column id = 'Origin' ifTrue: [ ^ self originColumn: column row: rowIndex ].
	
	self error: 'Invalid column'