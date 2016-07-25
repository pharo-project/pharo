unaccepted edits
configureTextMorph: aTextMorph
	"I prepare a text morph for use"
	aTextMorph setEditView: self.
	scroller addMorph: aTextMorph.
	aTextMorph 
		autoAccept: self autoAccept;
		selectionColor: self selectionColor.
	aTextMorph editor installKeymappingsOn: self.
