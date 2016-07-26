accessing
selectionColor: aColor
	selectionColor := aColor.
	paragraph ifNotNil: [:p | p selectionColor: aColor]