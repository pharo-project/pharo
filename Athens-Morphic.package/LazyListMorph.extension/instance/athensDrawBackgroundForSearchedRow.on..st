*Athens-Morphic
athensDrawBackgroundForSearchedRow: row on: aCanvas 

	aCanvas setPaint: listSource  secondarySelectionColor.
	aCanvas drawShape: (self selectionFrameForRow: row)