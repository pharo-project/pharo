visual properties
fillStyle: aColor
	undoProperties ifNil: [undoProperties := selectedItems collect: [:m | m fillStyle]].
	selectedItems do: [:m | m fillStyle: aColor]