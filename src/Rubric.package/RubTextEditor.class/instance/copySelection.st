menu messages
copySelection
	"Copy the current selection and store it in the paste buffer, unless a caret.  Undoer & Redoer: undoCutCopy"

	self lineSelectAndEmptyCheck: [^ self].
	self clipboardTextPut: self selection.
	self editingState previousInterval: self selectionInterval.
