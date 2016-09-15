accessing - rendering
centerOfColumnIndex: columnIndex
	^ (self leftOfColumnIndex: columnIndex) + (self columnSize // 2)