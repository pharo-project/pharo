accessing
editPostscript
	"edit the receiver's postscript, in a separate window.  "
	self assurePostscriptExists.
	UIManager default
		edit: self postscript 
		label: 'Postscript for ChangeSet named ', name
		accept:[:aString| self postscript: aString].