accessing
cellColumn: column row: rowIndex 
	| element cell |
	
	element := self elementAt: rowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		addMorph: (self 
			rowMorphForColumn: column glamourColumn 
			element: element);
		yourself.

	(self table columns first == column) ifTrue: [
		| icon |
		icon := self glamourPresentation icon cull: element.
		icon ifNotNil: [ cell addMorph: icon asMorph ] ].

	^ cell