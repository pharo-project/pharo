initialization
initialize
	"Initialize the receiver."

	super initialize.
	self beResizeable.
	entryText := ''.
	self changed: #entryText.
	self textEditorMorph selectAll.