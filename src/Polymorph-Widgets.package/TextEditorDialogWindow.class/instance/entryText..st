accessing
entryText: anObject
	"Set the value of entryText"

	entryText := anObject.
	self changed: #entryText.
	self textEditorMorph selectAll.
	self ok.
	self textEditorMorph hasUnacceptedEdits: true.