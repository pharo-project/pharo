updating
slideIndexBy: delta andMoveTopTo: newTop
	"Relocate my character indices and y-values.
	Used to slide constant text up or down in the wake of a text replacement."

	firstIndex := firstIndex + delta.
	lastIndex := lastIndex + delta.
	bottom := bottom + (newTop - top).
	top := newTop.
