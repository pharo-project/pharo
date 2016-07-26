accessing
cellColumn: column row: rowIndex
	| item cell |
	item := self elementAt: rowIndex.
	cell := FTIndentedCellMorph new.
	cell indentBy: item depth * 16.
	
	cell addMorphBack: (self buttonFor: item).
	cell addMorphBack: (self toString: item data) asMorph.
	^ cell