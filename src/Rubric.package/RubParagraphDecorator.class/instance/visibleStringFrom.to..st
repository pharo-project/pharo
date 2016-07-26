accessing
visibleStringFrom: topLeft to: bottomRight
	| topLeftBlk bottomRightBlk startIdx stopIdx string  |
	topLeftBlk := self characterBlockAtPoint: topLeft.
	bottomRightBlk := self characterBlockAtPoint: bottomRight.
	startIdx := topLeftBlk stringIndex.
	stopIdx := bottomRightBlk stringIndex.
	string := self text string.
	^ self text string copyFrom: startIdx to: (stopIdx min: string size)
