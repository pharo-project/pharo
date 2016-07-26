accessing
borderWidth: aWidth

	| bordered |
	bordered := selectedItems select: [:m | m isKindOf: BorderedMorph].
	undoProperties ifNil: [undoProperties := bordered collect: [:m | m borderWidth]].
	bordered do: [:m | m borderWidth: aWidth]