editing
handleEdit: editBlock
	"Ensure that changed areas get suitably redrawn"
	editBlock value.
	self selectionChanged.  "Note new selection"
