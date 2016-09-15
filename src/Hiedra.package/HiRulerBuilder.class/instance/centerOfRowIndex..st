accessing - rendering
centerOfRowIndex: rowIndex
	^ (self topOfRowIndex: rowIndex) + (self rowSize // 2)