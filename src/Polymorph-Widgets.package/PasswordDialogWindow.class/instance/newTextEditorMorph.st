creation
newTextEditorMorph
	"Answer a new morph for the text entry using a password font."

	textEditor := super newTextEditorMorph.
	self beEncrypted.
	^ textEditor