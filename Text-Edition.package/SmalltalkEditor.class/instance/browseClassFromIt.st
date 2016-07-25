menu messages
browseClassFromIt
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."
	
	self lineSelectAndEmptyCheck: [^ self].
	self browseClassFrom: (self selection string copyWithoutAll: CharacterSet crlf)