row management
selectedRow: index
	"Select the index-th row.  if nil, remove the current selection."
	
	selectedRow ifNotNil: [self selectionFrameChanged].
	selectedRow := index.
	selectedRow ifNotNil: [self selectionFrameChanged]