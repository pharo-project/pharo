selection
scrollSelectionToRow: row
	"make sure that the current selection is visible"

	row = 0 ifTrue: [ ^ self ].
	self scrollToShow: (self listMorph drawBoundsForRow: row)