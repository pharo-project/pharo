undo
borderWidthForItems: widthCollection

	(selectedItems select: [:m | m isKindOf: BorderedMorph])
		with: widthCollection
		do: [:m :c | m borderWidth: c]