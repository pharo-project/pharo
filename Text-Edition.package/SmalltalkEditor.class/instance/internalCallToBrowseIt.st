menu messages
internalCallToBrowseIt
	"Launch a browser for the class indicated by the current selection. 
	If multiple classes matching the selection exist, let the user choose among them."
	| aClass |
	self lineSelectAndEmptyCheck: [^ self].
	self internalCallToBrowse: (self selection string copyWithoutAll: CharacterSet crlf)