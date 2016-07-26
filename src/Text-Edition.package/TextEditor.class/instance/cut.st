private
cut
	"Cut out the current selection and redisplay the paragraph if necessary.  Undoer & Redoer: undoCutCopy:"

	self lineSelectAndEmptyCheck: [^ self].
	self clipboardTextPut: self selection.
	self replaceSelectionWith: self nullText.
