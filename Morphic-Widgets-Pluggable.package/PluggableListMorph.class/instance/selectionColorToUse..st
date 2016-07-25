accessing
selectionColorToUse: aColor
	"Set the colour for selected items."

	aColor = self selectionColorToUse ifTrue: [^self].
	selectionColorToUse := aColor.
	self listMorph selectionFrameChanged