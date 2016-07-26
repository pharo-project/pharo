*Athens-Morphic
athensDrawBackgroundForRow: row on: aCanvas color: aColor

	aCanvas setPaint: aColor.
	aCanvas drawShape: (self selectionFrameForRow: row)