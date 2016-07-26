moving changes
editPreamble
	"edit the receiver's preamble, in a separate window.  "
	self assurePreambleExists.
	UIManager default
		edit: self preamble 
		label: 'Preamble for ChangeSet named ', name
		accept:[:aString| self preamble: aString]