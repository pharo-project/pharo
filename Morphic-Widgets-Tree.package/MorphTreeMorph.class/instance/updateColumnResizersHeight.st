updating
updateColumnResizersHeight
	self columnResizers do: [:col | | b |
		b :=  col bounds.
		b := b left @ self top corner: b right @ scroller bottom.
		col bounds: b].
