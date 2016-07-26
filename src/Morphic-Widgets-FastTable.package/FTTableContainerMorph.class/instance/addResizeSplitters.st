drawing
addResizeSplitters
	| columnWidths nextColumn delta |
	columnWidths := self calculateColumnWidths.
	nextColumn := self left.
	delta := FTColumnResizerMorph resizerWidth / 2.
	self owner columns overlappingPairsWithIndexDo:  [ :leftColumn :rightColumn :index |
		nextColumn := nextColumn + (columnWidths at: index) + self owner intercellSpacing x.
		self addMorph: ((FTColumnResizerMorph 
			container: self 
				left: (FTDisplayColumn column: leftColumn width: (columnWidths at: index))
				right: (FTDisplayColumn column: rightColumn width: (columnWidths at: (index + 1))))
			bounds: ((nextColumn - delta)@(self top) extent: delta@(self height));
			color: Color transparent;
			yourself)	 ]