editing
handleEdit: editBlock
	"Ensure that changed areas get suitably redrawn"
	self selectionChanged.  "Note old selection"
		editBlock value.
	self selectionChanged.  "Note new selection"
	self updateFromParagraph  "Propagate changes as necessary"