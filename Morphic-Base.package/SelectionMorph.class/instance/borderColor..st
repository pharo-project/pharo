accessing
borderColor: aColor

	| bordered |
	bordered := selectedItems.
	undoProperties ifNil: [undoProperties := bordered collect: [:m | m borderColor]].
	bordered do: [:m | m borderColor: aColor]