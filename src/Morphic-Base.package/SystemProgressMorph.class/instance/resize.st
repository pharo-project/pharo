updating
resize

	| newWidth |
	newWidth := self maxBarWidth + 50 max: self width.
	self width: newWidth.