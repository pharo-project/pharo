undo
borderColorForItems: colorCollection

	(selectedItems select: [:m | m isKindOf: BorderedMorph])
		with: colorCollection
		do: [:m :c | m borderColor: c]