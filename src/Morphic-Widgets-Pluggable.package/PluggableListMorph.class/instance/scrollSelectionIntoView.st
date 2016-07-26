selection
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row |
	row := self getCurrentSelectionIndex.
	self scrollSelectionToRow: row