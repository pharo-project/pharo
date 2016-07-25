testing
isRowIndexExceding: rowIndex
	| headerPresentModificator nextRowIndexByPosition heightWithSpacing |
	
	headerPresentModificator := headerRow ifNotNil: [ 1 ] ifNil: [ 0 ].
	nextRowIndexByPosition := rowIndex - self owner showIndex + 1 + headerPresentModificator.
	heightWithSpacing := self owner rowHeight + self owner intercellSpacing y.
	
	^ (nextRowIndexByPosition * heightWithSpacing) > self height