accessing
cellColumn: column row: rowIndex
	| cell |
	cell := FTCellMorph new
		addMorphBack: (self elementAt: rowIndex) asMorph;
		yourself.
	(self needSeparatorBefore: rowIndex)
		ifTrue: [ cell withTopSeparator ].
	^ cell