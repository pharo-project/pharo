undo
fillStyleForItems: fillStyleCollection

	selectedItems with: fillStyleCollection do: [:m :c | m fillStyle: c]