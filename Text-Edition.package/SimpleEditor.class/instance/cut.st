menu messages
cut
	"Cut out the current selection and redisplay the paragraph if necessary.  Undoer & Redoer: undoCutCopy:"

	self lineSelectAndEmptyCheck: [^ self].
	self replaceSelectionWith: ''. 
	self clipboardTextPut: self selection