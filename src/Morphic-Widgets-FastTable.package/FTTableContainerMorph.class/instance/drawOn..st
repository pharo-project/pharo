drawing
drawOn: canvas
	| x y cellWidth cellHeight rowsToDisplay rowSubviews highligtedRowIndexes primarySelectionIndex |
	
	super drawOn: canvas.
	self canRefreshValues ifFalse: [ ^ self ]. "Nothing to update yet"

	x := self left + self class rowLeftMargin.
	y := self top.	
	cellWidth := self width - self class rowLeftMargin.
	cellHeight := self owner rowHeight rounded. 
	highligtedRowIndexes := 
		self owner selectedRowIndexes, 
		self owner highlightedRowIndexes.
	primarySelectionIndex := self owner selectedRowIndex.
	
	"For some superweird reason, calling #calculateExposedRows here instead in #changed (where
	 it should be called) is 10x faster. Since the whole purpose of this component is speed, for 
	 now I'm calling it here and adding the #setNeedRecalculateRows mechanism. 
	 History, please forgive me."
	self updateAllRows.

	rowsToDisplay := self exposedRows.
	rowSubviews := OrderedCollection new: rowsToDisplay size + 1.
	headerRow ifNotNil: [ 
		headerRow bounds: (self left@y extent: self width@cellHeight).
		y := y + cellHeight + self owner intercellSpacing y.		
		rowSubviews add: headerRow ].
	
	rowsToDisplay keysAndValuesDo: [ :rowIndex :row | | visibleHeight |
		visibleHeight := cellHeight min: (self bottom - y). 
		row bounds: (x@y extent: cellWidth@visibleHeight).
		y := y + visibleHeight + self owner intercellSpacing y.
		
		(highligtedRowIndexes includes: rowIndex) ifTrue: [ 
			row selectionColor: (self owner colorForSelection: primarySelectionIndex = rowIndex) ].
		rowSubviews add: row ].
						
	submorphs := rowSubviews asArray.
	
	owner isResizable 
		ifTrue: [ self addResizeSplitters ].
	

	needsRefreshExposedRows := false