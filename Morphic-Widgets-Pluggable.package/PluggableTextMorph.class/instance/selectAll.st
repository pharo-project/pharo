editor access
selectAll
	"Tell my textMorph's editor to select all"

	self textMorph editor selectAll.
	self selectionInterval: self textMorph editor selectionInterval